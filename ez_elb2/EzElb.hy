(import [troposphere [Template]])
(import [troposphere.s3 [Bucket]])
(import inspect)
(import pprint)

(defclass ValidationException [Exception]
  (defn --init-- [self message]
    (.--init-- (super Exception self) message)))

;;
;; We will refer to the data structure which evolves into the template
;; as an EDef, short for ELB Definition.
;;

(defn init-edef [sceptre_user_dat]
  "returns a skeleton EDef"
  {:sceptre-user-dat sceptre_user_dat
   :config {} })

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
  "returns a list of single-entry maps suitable for specifying AWS tags from a list of paired strings"
  (list (map (fn [p] (dict [p])) (partition l))))

(defn arity [f]
  "returns the arity of the given function"
  (len (get (inspect.getargspec f) 0)))

(defn args->fns [args]
  "Returns a generator which yields arity 1 functions for mutating the
  EDef. It's primary purpose is to handle keywords."

  (while (not (empty? args))
    (setv head (first (pop-head args)))
    (if (keyword? head)
        ;; Convert the keyword into a function and pop it's arguments
        (do (setv f (kw->fn head))
            (yield (apply f (pop-head args (arity f)))))

        ;; Not a keyword, must be a function so leave it alone.
        (yield head))))

(defn ez-elb-f [sceptre_user_dat args dump-def]
  "This is the heart of EZ-ELB. It processes it's arguments and produces a template."

  ;; Build an empty, default edef
  (setv edef (init-edef sceptre_user_dat))

  ;; Process args to create the edef desired by the user
  (for [f (args->fns args)] (apply f [edef]))

  (if dump-def (pprint.pprint edef))
  
  (setv template (Template))
  (.add_resource template (Bucket "SomeBucket"))
  (.to_json template))

(defmacro ez-elb [&rest args]
  `(do (defn sceptre_handler [sceptre_user_dat &optional dump-def]
         (ez-elb-f sceptre_user_dat ~args dump-def))
       (defmain [&rest args] (sceptre_handler None True))))

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
  
  `(setv ~(HySymbol (+ "ez-elb-kw-" (name user-kw)))  ; should be able to use kw->fname but it didn't work
         (fn [~g!v] ~desc
           (fn [~g!edef]
             (assoc (. ~g!edef [:config]) ~conf-kw (~xform ~g!v))))))

(defn ez-elb-kw-no-op []
  "no-op keyword function"
  (fn []))

(defn ez-elb-kw-name [n]
  "set the name of the ELB"
  (fn [edef]
    (assoc (. edef [:config]) :elb-name n)))

;;
;; Keyword Definitions
;;
(defkw-kv [:name :elb-name] "the name of the ELB")
(defkw-kv :subnet-ids "the subnet IDs")
(defkw-kv :vpc "the VPC for the ELB")
(defkw-kv :certificate-id "certificate for the ELB")
(defkw-kv :alarm-topic "SMS topic where CloudWatch alarms will be sent")
(defkw-kv :log-bucket "ELB logs will be sent to this bucket")
(defkw-kv :global-tags "a list of tags to assign to all taggable resources in key/value pairs" list-pairs->tag-list)

