(import [ez_elb2.EzElb [*]]
        [native_tests.util [*]]
        pytest
        json
        yaml)
(require [ez_elb2.EzElb [*]])

(defn edef->test-template [edef]

  "runs an edef through edef->template but instead of JSON it returns
  a more inspect-able representation of the CF template"

  (print "------ EDEF ------")
  (print (.pformat (EzPrettyPrinter) edef))
  (print "------------------")

  (setv ret (->> (edef->template edef)
                 (json.loads)      ; JSON  -> Python object with special references
                 (yaml.safe_dump)  ; PyObj -> YAML w/o special references
                 (yaml.load)))     ; YAML  -> Python object without special references

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
  
  (assert= 0 1))
