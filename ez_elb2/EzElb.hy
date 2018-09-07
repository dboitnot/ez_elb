(import [troposphere [Template]]
        [troposphere.s3 [Bucket]]
        inspect
        pprint
        collections
        threading
        logging)

(setv *log* (logging.getLogger (+ "sceptre." --name--)))

(defclass ValidationException [Exception]
  (defn --init-- [self message]
    (.--init-- (super Exception self) message)))


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
  "removes and returns the first n (default 1) values off the HEAD of a collection"
  (setv ret (cut col 0 n))
  (del (cut col 0 n))  
  ret)

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

(defn ez-elb-f [edef]
  (*log*.debug "Post-body ELB-Def:\n\n%s\n" (pprint.pformat edef))

  (setv template (Template))
  (.add_resource template (Bucket "SomeBucket"))
  (.to_json template))

(defn args->fns [args]
  (while (not (empty? args))
    (setv head (first (pop-head args)))   
    (if (keyword? head)

        ;; Convert keywords into function calls
        ;; TODO: Wrap symbol generation as kw->sym
        (do (setv f (kw->fn head))
            (yield (HyExpression (+ [(HySymbol (+ "ez-elb-kw-" (name head)))] (pop-head args (arity f))))))

        ;; Yield the rest unmodified
        (yield head))))

(defmacro ez-elb [&rest args]  
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

(defn target [host port path &optional [protocol "HTTP"]]
  "Defines a target for a path."

  (fn [edef]
    (.append (get edef :target-paths path)
             {:host host
              :port port
              :protocol protocol})))

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
