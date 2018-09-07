(import [troposphere [Template]]
        [troposphere.s3 [Bucket]]
        [functools [partial]]
        inspect
        pprint
        collections
        threading
        copy
        logging)

(setv *log* (logging.getLogger (+ "sceptre." --name--)))

(defclass ValidationException [Exception]
  (defn --init-- [self message]
    (.--init-- (super Exception self) message)))

(defclass PrettyRepr [object]
  (defn --init-- [self obj]
    (setv self.obj obj))          
  
  (defn --repr-- [self]
    (cond [(keyword? self.obj) (+ ":" (name self.obj))]
          [True (.--repr-- self.obj)])))

(defclass EzPrettyPrinter [pprint.PrettyPrinter object]
  (defn format [self obj ctx mxl lvl]
    (setv new-obj
          (cond [(keyword? obj) (PrettyRepr obj)]
                [True obj]))
    (.format (super EzPrettyPrinter self) new-obj ctx mxl lvl)))

;; A thread-local object used to provide context to functions within
;; an ez-elb macro
(setv *context* (threading.local))

;;
;; Utility Functions
;;

(defn kw->fname [kw]
  (+ "ez-elb-kw-" (name kw)))

(defn kw->fn [kw]
  "returns the function associated with the keyword"
  (setv d (globals))
  (setv f-name (mangle (kw->fname kw)))
  (if (in f-name d)
      (get d f-name)
      (raise (NameError (+ "Unknown EZ-ELB keyword :" (name kw))))))

(defn pop-head [col &optional [n 1]]
  
  "Removes and returns the first n (default 1) values off the HEAD of
  a collection, always returns a list."
  
  (setv ret (cut col 0 n))
  (del (cut col 0 n))  
  ret)

(defn pop-head! [col]
  "Removes and returns the first item from a collection"
  (get (pop-head col) 0))

(defn list-pairs->tag-list [l]
  "returns a list of maps suitable for specifying AWS tags from a list
  of paired strings"

  (list (map (fn [p] {"Key" (get p 0) "Value" (get p 1)}) (partition l))))

(defn arity [f]
  "returns the arity of the given function"
  (len (get (inspect.getargspec f) 0)))

;;
;; EZ-ELB Core Functions & Macros
;;
;; We will refer to the data structure which evolves into the template
;; as an EDef, short for ELB Definition.
;;

(defn init-edef [sceptre_user_dat]
  "returns a skeleton EDef"
  {:sceptre-user-dat sceptre_user_dat
   :config {}
   :target-paths (collections.defaultdict (fn [] []))})

(defn args->fns [args]

  "This method is used by the ez-elb macro to process keywords in it's
  body into function calls. It returns a generator which yields Hy
  expressions representing the body of the macro."
  
  (while (not (empty? args))
    (setv head (first (pop-head args)))   
    (if (keyword? head)

        ;; Convert keywords into function calls
        ;; TODO: Wrap symbol generation as kw->sym
        (do (setv f (kw->fn head))
            (yield (HyExpression (+ [(HySymbol (+ "ez-elb-kw-" (name head)))] (pop-head args (arity f))))))

        ;; Yield the rest unmodified
        (yield head))))

(defn ez-elb-f [edef]
  
  "This function is called by the ez-elb macro after it's body has
  been processed. It's job is to take the edef generated in the body,
  validate it, and return a JSON template."

  ;; At this point it's safe to convert the defaultdicts into regular
  ;; dicts for pretty printing. This seems to be the only reliable way
  ;; to get readable edef dumps without re-writing pprint.
  (assoc edef :target-paths (dict (:target-paths edef)))
  
  (*log*.debug "Post-body ELB-Def:\n\n%s\n" (.pformat (EzPrettyPrinter) edef))

  (setv template (Template))
  (.add_resource template (Bucket "SomeBucket"))
  (.to_json template))

(defmacro ez-elb [&rest args]
  "This is the core macro for defining an EZ-ELB."
  
  `(do
     ;; Build the sceptre handler
     ~(+ '(defn sceptre_handler [sceptre_user_dat]
            ;; Build an initial edef and put it in the thread-local context
            (setv edef (init-edef sceptre_user_dat))
            (setv *context*.edef edef))

         ;; Handle the body of the macro
         (list (args->fns (list args)))

         ;; Do the rest of the processing in a proper function
         ['(ez-elb-f edef)])

     ;; Create a __main__ function for testing purposes
     (defmain [&rest args]
       (logging.basicConfig)
       (*log*.setLevel logging.DEBUG)
       (sceptre_handler None))))

(defn target-single [host port path protocol]
  
  "This is the underlying function for target. It defines a single
  path/target mapping."

  (.append (get *context*.edef :target-paths path)
           {:host host
            :port port
            :protocol protocol}))

(defn target-multi [hosts port-paths protocol org-form]

  "This is an intermediate function for the target macro. It handles
  the the possibility of multiple hosts and port/path pairs."

  (for [h hosts]
    (for [pp port-paths]
      (if (and (coll? pp) (= 2 (len pp)))
          (target-single h (get pp 0) (get pp 1) protocol)
          (raise (ValidationException (+ "invalid port/path expression in target definition: " org-form)))))))

(defmacro target [&rest args]

  "Defines a target for a path. It can be used in the following
  forms:

  (target host port path)
  (target [host1 host2 ...] port path)
  (target host [[port1 path1] [port2 path2] ...])
  (target [host1 host2 ...] [[port1 path1] [port2 path2]])

  The :http and :https keywords may be appended as the last argument
  to indicate the back-end protocol. :http is the default.

  Example:

  (target [host1 host2] port path :http)
  (target host [[port1 path1] [port2 path2]] :https)"

  ;; Preserve a the expression in case we have to report it in an
  ;; error message
  (setv org-form
        (+ "(target " (.join " " (map str args)) ")"))

  ;; We need to work with args as a mutable list
  (setv args (list args))

  ;; If the last arg is a protocol keyword, pop it off and update the
  ;; protocol value.
  (setv protocol "HTTP")
  (if (in (get args -1) [:http :https])
      (setv protocol (.upper (name (.pop args)))))

  ;; The first argument is always either a host or list of hosts. If
  ;; it's not already a collection, make it into one.
  (setv hosts (pop-head! args))
  (if (not (coll? hosts)) (setv hosts [hosts]))

  ;; At this point we should either have two args (port & path) or one
  ;; (a list of port/path pairs). Otherwise we have an invalid
  ;; expression.
  (setv port-paths (cond [(= 2 (len args)) [args]]
                         [(= 1 (len args)) (first args)]
                         [True (raise (ValidationException (+ "invalid target expression: " org-form)))]))

  `(target-multi ~hosts ~port-paths ~protocol ~org-form))

;;
;; Keyword Definitions
;;

(defmacro defkw [kw &rest args]
  "define a keyword function just like defn but specifying a keyword for the name"
  (+ `(defn ~(HySymbol (+ "ez-elb-kw-" (name kw)))) (list args)))

(defmacro/g! defkw-kv [kw desc &optional [xform 'identity]]

  "Define a simple key/value keyword function which sets the
  associated value in the :config map.

  If kw is a collection, it's first item will be the user-facing
  keyword and it's second will be the key used in the config dict.

  The user input will be passed through xform before being stored. By
  default xform is the identity function."

  (if (coll? kw)
      (do (setv user-kw (get kw 0))
          (setv conf-kw (get kw 1)))
      (do (setv user-kw kw)
          (setv conf-kw kw)))
  
  `(defkw ~user-kw [~g!v] ~desc     
     (assoc (get *context*.edef :config) ~conf-kw (~xform ~g!v))))

(defkw-kv [:name :elb-name] "the name of the ELB")
(defkw-kv :subnet-ids "the subnet IDs")
(defkw-kv :vpc "the VPC for the ELB")
(defkw-kv :certificate-id "certificate for the ELB")
(defkw-kv :alarm-topic "SMS topic where CloudWatch alarms will be sent")
(defkw-kv :log-bucket "ELB logs will be sent to this bucket")
(defkw-kv :global-tags "a list of tags to assign to all taggable resources in key/value pairs" list-pairs->tag-list)

(defkw :no-op [] "does nothing" (fn [_]))
