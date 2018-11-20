(import [ez_elb2.EzElb [*]]
        [native_tests.util [*]]
        pytest
        json
        yaml)
(require [ez_elb2.EzElb [*]])

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
  (setv tmp (edef->template edef))
  (print (yaml.safe_dump (json.loads tmp) :encoding "utf-8")))
