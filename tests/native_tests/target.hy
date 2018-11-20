(import [ez_elb2.EzElb [*]]
        collections)
(require [ez_elb2.EzElb [*]])

(import [native_tests.util [*]])

(defmacro target-paths [&rest args]
  `(do
     (setv *context*.edef {:target-paths (collections.defaultdict (fn [] []))})
     (target ~@args)
     (dict (get *context*.edef :target-paths))))

(defn test-target-single []
  (assert= (target-paths "host" 1234 "path")
           {"path" [{:host "host" :port 1234 :protocol "HTTP"}]}))

(defn test-target-multihost []
  (assert= (target-paths ["host1" "host2"] 1234 "path")
           {"path" [{:host "host1" :port 1234 :protocol "HTTP"}
                    {:host "host2" :port 1234 :protocol "HTTP"}]}))

(defn test-target-multihost-https []
  (assert= (target-paths ["host1" "host2"] 1234 "path" :https)
           {"path" [{:host "host1" :port 1234 :protocol "HTTPS"}
                    {:host "host2" :port 1234 :protocol "HTTPS"}]}))

(defn test-target-multiport-path []
  (assert= (target-paths "host" [[1234 "path1"] [1235 "path2"]])
           {"path1" [{:host "host" :port 1234 :protocol "HTTP"}]
            "path2" [{:host "host" :port 1235 :protocol "HTTP"}]}))

(defn test-target-multihost-multiport-path []
  (assert= (target-paths ["host1" "host2"] [[1234 "path1"] [1235 "path2"]])
           {"path1" [{:host "host1" :port 1234 :protocol "HTTP"}
                     {:host "host2" :port 1234 :protocol "HTTP"}]
            "path2" [{:host "host1" :port 1235 :protocol "HTTP"}
                     {:host "host2" :port 1235 :protocol "HTTP"}]}))

(defn test-target-invalid-syntax []
  (try
   (target-paths "host" [1234 "path"])
   (except [e ValidationException])
   (else (raise (Exception "expected ValidationException")))))
