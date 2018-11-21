(import [troposphere [Template Ref Sub GetAtt Output Export]]
        [troposphere.cloudwatch [Alarm MetricDimension]]
        [troposphere.ec2 [SecurityGroup SecurityGroupRule]]
        [troposphere.ecs [TaskDefinition ContainerDefinition Environment PortMapping Service
                          DeploymentConfiguration]]
        [troposphere.ecs [LoadBalancer :as EcsLoadBalancer]]
        [troposphere.elasticloadbalancingv2 [LoadBalancer Listener ListenerRule TargetGroup Certificate Action
                                             TargetDescription Condition Matcher TargetGroupAttribute LoadBalancerAttributes]]
        [troposphere.route53 [RecordSetGroup RecordSet]]
        [troposphere.s3 [Bucket]]
        [functools [partial]]        
        pprint
        collections
        threading
        copy
        yaml       
        json
        logging
        [ez_elb2.tropo [*]]
        [ez_elb2.util [*]]
        [ez-elb2.conf_kw [*]])

(require [ez_elb2.util [*]])

(setv *log* (logging.getLogger (+ "sceptre." --name--)))

(defclass ValidationException [Exception]
  (defn --init-- [self message]
    (.--init-- (super Exception self) message)))

(defclass PrettyRepr [object]
  (defn --init-- [self obj]
    (setv self.obj obj))          
  
  (defn --repr-- [self]
    (cond [(keyword? self.obj) (+ ":" (name self.obj))]
          [(instance? collections.defaultdict self.obj) (.--repr-- (dict self.obj))]
          [True (.--repr-- self.obj)])))

(defclass EzPrettyPrinter [pprint.PrettyPrinter object]
  (defn format [self obj ctx mxl lvl]
    (setv new-obj
          (cond [(keyword? obj) (PrettyRepr obj)]
                [(instance? collections.defaultdict obj) (PrettyRepr obj)]
                [True obj]))
    (.format (super EzPrettyPrinter self) new-obj ctx mxl lvl)))

;; A thread-local object used to provide context to functions within
;; an ez-elb macro
(setv *context* (threading.local))

;;
;; EZ-ELB Core Functions & Macros
;;
;; We will refer to the data structure which evolves into the template
;; as an EDef, short for ELB Definition.
;;

(defn init-edef [sceptre_user_dat]
  "returns a skeleton EDef"
  {:sceptre-user-dat sceptre_user_dat
   :config {:idle-timeout-seconds 120
            :healthy-threshold-count 2
            :healthy-http-codes "200-399"}
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

(defn add-output [template lname desc value &optional [export-name None]]
  "Add an export to the template"

  (setv ret
        (Output
          lname
          :Description desc
          :Value value))

  (if export-name
      (setv ret.Export (Export export-name)))

  (.add-output template ret)
  ret)

(defn add-sg [edef template lname desc ingress &optional [tag-name None] [output-desc None] [export-name None]]

  "Adds a security group to the template with the given logical name,
   description and ingress rules.
   
   ingress should be a list of lists which can be passed to
   SecurityGroupRule as arugments.

   If tag-name is unspecified it will be set to desc"

  (setv tag-name (or tag-name desc))

  (setv ret
        (SecurityGroup
          lname
          :GroupDescription desc
          :Tags [{"Name" tag-name}]
          :VpcId (get edef :config :vpc)
          :SecurityGroupEgress [(SecurityGroupRule :CidrIp "0.0.0.0/0" :IpProtocol "-1")]
          :SecurityGroupIngress (list (map (fn [a] (apply-kw SecurityGroupRule [] a)) ingress))))
  (.add-resource template ret)

  (if output-desc
      (add-output template (+ lname "Output") output-desc (Ref ret) export-name))
  
  ret)

(defn add-elb-sg [edef template]
  "Adds the security group for the ELB"

  (setv ret (add-sg edef template "ElbSecurityGroup" (Sub "${AWS::StackName}-ElbSg")
                    
                    [{:CidrIp "0.0.0.0/0" :IpProtocol "tcp" :FromPort 443 :ToPort 443}
                     {:CidrIp "0.0.0.0/0" :IpProtocol "tcp" :FromPort 80 :ToPort 80}]
                    
                    :output-desc "Security group ID assigned to the ELB"
                    :export-name (Sub "${AWS::StackName}-ElbSg")))
  ret)

(defn add-inst-sg [edef template]
  "Adds a convenience security group to the ELB to assign instances to"

  (setv ret (add-sg edef template "InstanceSecurityGroup" (Sub "${AWS::StackName}-InstSg")

                    [{:IpProtocol "-1" :SourceSecurityGroupId (Ref "ElbSecurityGroup")}]

                    :output-desc "Convenience SG to assign to instances"
                    :export-name (Sub "${AWS::StackName}-InstSg"))))

(defn elbv2-attributes [edef]
  "returns a list of LoadBalancerAttributes based on the EDEF"

  (setv ret [(LoadBalancerAttributes :Key "idle_timeout.timeout_seconds"
                                     :Value (str (get edef :config :idle-timeout-seconds)))])

  (if-get edef log-bucket [:config :log-bucket]
          (.extend ret [(LoadBalancerAttributes :Key "access_logs.s3.enabled" :Value "true")
                        (LoadBalancerAttributes :Key "access_logs.s3.bucket" :Value log-bucket)
                        (LoadBalancerAttributes :Key "access_logs.s3.prefix" :Value (Sub "${AWS::StackName}-ElbLogs"))]))

  ret)

(defn add-elb [edef template lname elb-name security-groups &optional [subnets None] [tag-name None]]
  
  "Add an application load-balancer to the template."

  (setv subnets (or subnets (get edef :config :subnet-ids)))
  (setv tag-name (or tag-name elb-name))

  (setv ret
        (LoadBalancer
          lname
          :Name elb-name
          :SecurityGroups security-groups
          :Subnets subnets
          :Tags [{"Name" tag-name}]
          :LoadBalancerAttributes (elbv2-attributes edef)))

  (.add-resource template ret)

  ret)

(defn add-main-elb [edef template]
  (setv ret (add-elb edef template "ELB" (Ref "AWS::StackName")
                     [(Ref "ElbSecurityGroup")]))
  (add-output template "ElbArnOutput" "ARN of the ELB" (Ref ret) (Sub "${AWS::StackName}-ElbArn"))
  (add-output template "ElbDnsOutput" "DNS name of the ELB" (GetAtt "ELB" "DNSName") (Sub "${AWS::StackName}-ElbDns"))

  ret)

(defn validate-edef [edef]
  ;; Check for required conf keywords
  (setv conf (:config edef))
  (for [kwd CONF_KEYWORDS]
    (if (:required kwd)
        (unless (in (:conf-kw kwd) conf)
          (raise (ValidationException
                   (.format "Missing required keyword {} ({})"
                            (:user-kw kwd)
                            (:desc kwd))))))))

(defn edef->template [edef]
  (validate-edef edef)
  
  (setv template (Template))

  (add-elb-sg edef template)
  (add-inst-sg edef template)
  (add-main-elb edef template)
  
  (.to_json template))

(defn ez-elb-f [edef]
  
  "This function is called by the ez-elb macro after it's body has
  been processed. It's job is to take the edef generated in the body,
  validate it, and return a JSON template."

  ;; At this point it's safe to convert the defaultdicts into regular
  ;; dicts for pretty printing. This seems to be the only reliable way
  ;; to get readable edef dumps without re-writing pprint.
  (assoc edef :target-paths (dict (:target-paths edef)))
  (*log*.debug "Post-body ELB-Def:\n\n%s\n" (.pformat (EzPrettyPrinter) edef))

  (setv ret (edef->template edef))

  (*log*.debug "Template:\n\n%s\n%s%s\n"
               (* "-" 80)
               (yaml.safe_dump (json.loads ret) :encoding "utf-8")
               (* "-" 80))

  (*log*.debug "Post-build ELB-Def:\n\n%s\n" (.pformat (EzPrettyPrinter) edef))

  ret)

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

(defn pop-target-protocol [args]

  "Removes and returns the protocol at the end of args if there, otherwise returns HTTP"  
  
  ;; If the last arg is a protocol keyword, pop it off and update the
  ;; protocol value.
  (if (in (get args -1) [:http :https])
      (.upper (name (.pop args)))
      "HTTP"))

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
  (setv protocol (pop-target-protocol args))

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
