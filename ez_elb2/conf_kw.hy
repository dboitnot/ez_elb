;; The CONF_KEYWORDS global is used by validation functions and is
;; populated as keywords are defined.
(setv CONF_KEYWORDS [])

(defn kw->fname [kw]
  (+ "ez-elb-kw-" (name kw)))

(defn kw->fn [kw]
  "returns the function associated with the keyword"
  (setv d (globals))
  (setv f-name (mangle (kw->fname kw)))
  (if (in f-name d)
      (get d f-name)
      (raise (NameError (+ "Unknown EZ-ELB keyword :" (name kw))))))

;;
;; Keyword Definitions
;;
(defn reg-conf-kw [user-kw conf-kw desc required]
  (.append CONF_KEYWORDS
           { :user-kw user-kw
            :conf-kw conf-kw
            :desc desc
            :required required}))

(defmacro defkw [kw &rest args]
  "define a keyword function just like defn but specifying a keyword for the name"
  (+ `(defn ~(HySymbol (+ "ez-elb-kw-" (name kw)))) (list args)))

(defmacro/g! defkw-kv [kw desc &optional [required False] [xform 'identity]]

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
  
  `(do
     (apply reg-conf-kw [~user-kw ~conf-kw ~desc ~required])
     (defkw ~user-kw [~g!v] ~desc     
       (assoc (get *context*.edef :config) ~conf-kw (~xform ~g!v)))))

(defkw-kv [:name :elb-name] "the name of the ELB" True)
(defkw-kv :subnet-ids "the subnet IDs" True)
(defkw-kv :vpc "the VPC for the ELB" True)
(defkw-kv :certificate-id "certificate for the ELB")
(defkw-kv :alarm-topic "SMS topic where CloudWatch alarms will be sent")
(defkw-kv :log-bucket "ELB logs will be sent to this bucket")
(defkw-kv :global-tags "a list of tags to assign to all taggable resources in key/value pairs"
  False list-pairs->tag-list)

(defkw :no-op [] "does nothing" (fn [_]))
