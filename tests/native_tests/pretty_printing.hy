(import [ez_elb2.EzElb [*]])
(require [ez_elb2.EzElb [*]])

(defn test-pp-kw []
  "test pretty printing of keywords"
  (assert (= (.pformat (EzPrettyPrinter) (-> :test)) ":test")))

(defn test-pp-str []
  "test pretty printing of strings"
  (assert (= (.pformat (EzPrettyPrinter) "test") "u'test'")))

(defn test-PrettyRepr-fall-through []
  "test fall-through of __repr__ in PrettyRepr"
  (assert (= (.--repr-- (PrettyRepr "test")) "u'test'")))
