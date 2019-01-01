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
  (print "-------------------")

  ret)

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

      (setv *context*.edef empty-edef)
      
      empty-edef))

(defn test-empty [empty-edef]
  (try
    (edef->template empty-edef)
    (except [e ValidationException])
    (else (raise (Exception "expected ValidationException")))))

(defn test-minimum [edef]
  (setv tmp (edef->test-template edef))
  (assert-resource tmp "ELB" "AWS::ElasticLoadBalancingV2::LoadBalancer"
                   (p= "Subnets" ["subnetid1" "subnetid2"]))
  (assert-resource tmp "ElbSecurityGroup" "AWS::EC2::SecurityGroup")
  (assert-resource tmp "InstanceSecurityGroup" "AWS::EC2::SecurityGroup")
  (assert-resource tmp "DefaultTargetGroup" "AWS::ElasticLoadBalancingV2::TargetGroup"
                   (p= "HealthyThresholdCount" 2)
                   (p= ["Matcher" "HttpCode"] "200-399")
                   (p= "Port" 8080)
                   (p= "Protocol" "HTTP")
                   (p= "Targets" [{"Id" "deftarget" "Port" 1234}]))
  (assert-resource tmp "HttpsListener" "AWS::ElasticLoadBalancingV2::Listener"))

(defn test-target [edef]
  (target-single "somehost" 1234 "somepath" "HTTPS")

  (setv tmp (edef->test-template edef))

  #_(assert-resource tmp "PathRlsomepath" "AWS::ElasticLoadBalancingV2::ListenerRule")
  #_(assert-resource tmp "PathRlnsomepath" "AWS::ElasticLoadBalancingV2::ListenerRule")
  #_(assert-resource tmp "PathTgsomepath" "AWS::ElasticLoadBalancingV2::TargetGroup"))
