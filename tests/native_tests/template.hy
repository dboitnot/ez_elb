(import [ez_elb2.EzElb [*]]
        [native_tests.util [*]]
        pytest
        json
        yaml)
(require [ez_elb2.EzElb [*]])

(defn dumpable [obj]

  "converts obj to something easy to read in debug output"

  (cond [(instance? dict obj) (dict)]
        [(iterable? obj) (list (map dumpable obj))]
        [(keyword? obj) (name obj)]
        [True obj]))

(defn edef->test-template [edef]

  "runs an edef through edef->template but instead of JSON it returns
  a more inspect-able representation of the CF template"

  (print "------ EDEF ------")
  (print (yaml.safe_dump (dumpable edef)))
  (print "------------------")

  (setv ret (->> (edef->template edef)
                 (json.loads)      ; JSON  -> Python object with special references
                 (yaml.safe_dump)  ; PyObj -> YAML w/o special references
                 (yaml.load)))     ; YAML  -> Python object without special references

  ;; Print a readable (YAML) representation. Pytest will only show
  ;; this after an error.
  (print "--- CF Template ---")
  (print (yaml.safe_dump ret))
  (print "-------------------"))

#@(pytest.fixture
    (defn empty-edef []
      (init-edef None)))

#@(pytest.fixture
    (defn edef [empty-edef]
      (setv conf (:config empty-edef))
      (assoc conf
             :elb-name "elbname"
             :vpc "vpcid"
             :subnet-ids ["subnetid1" "subnetid2"])
      empty-edef))

(defn test-empty [empty-edef]
  (try
    (edef->template empty-edef)    
    (except [e ValidationException])
    (else (raise (Exception "expected ValidationException")))))

(defn test-minimum [edef]
  (setv tmp (edef->test-template edef))
  
  #_(assert= 0 1))
