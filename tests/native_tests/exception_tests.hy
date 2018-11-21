(import [ez_elb2.EzElb [*]])
(require [ez_elb2.EzElb [*]])

(defn test-ValidationException []
  (assert (= (str (ValidationException "testmessage")) "testmessage")))
