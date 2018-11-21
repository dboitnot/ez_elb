(import [ez_elb2.EzElb [*]])
(require [ez_elb2.util [*]])

(import [native_tests.util [*]])

(defn test-kw-to-fname []
  (assert= (kw->fname "test") "ez-elb-kw-test"))

(defn test-dict-keys-kw-to-str []
  (setv kd { :test "west" :jest "fest" })
  (setv xp { "test" "west" "jest" "fest" })
  (setv got (dict-keys-kw->str kd))
  (assert= got xp))

(defn test-apply-kw []
  (apply-kw (fn [p1 p2 &optional kw1 kw2]
              (assert= p1 "p1val")
              (assert= p2 "p2val")
              (assert= kw1 "kw1val")
              (assert= kw2 "kw2val"))
            ["p1val" "p2val"]
            { :kw1 "kw1val" :kw2 "kw2val" }))

(defn test-pop-head []
  (setv tst (fn [args ret rem]
              (setv col [1 2 3])
              (assert= (apply (partial pop-head col) args) ret)
              (assert= col rem)))
  (tst [] [1] [2 3])
  (tst [1] [1] [2 3])
  (tst [2] [1 2] [3])
  (tst [3] [1 2 3] []))

(defn test-pop-head-bang []
  (setv col [1 2 3])
  (assert= (pop-head! col) 1)
  (assert= col [2 3]))

(defn test-list-pairs-to-tag-list []
  (assert= (list-pairs->tag-list ["test" "west" "jest" "fest"])
           [{"Key" "test" "Value" "west"}
            {"Key" "jest" "Value" "fest"}]))

(defn test-arity []
  (assert= (arity (fn [] pass)) 0)
  (assert= (arity (fn [a b] pass)) 2))

(defn test-if-get []
  (setv col { :a { :a "aa" :b "ab" }
             :b { :a "ba" :b "bb" } })

  ; Test success with only if-true body
  (assert= (if-get col v [:a :b] (do (assert= v "ab") "true-called")) "true-called")

  ; Test success with both bodies
  (assert= (if-get col v [:b :a] (do (assert= v "ba") "true-called") "false-called") "true-called")

  ; Test failure with only if-true body
  (assert= (if-get col v [:a :c] "true-called") None)

  ; Test failure with both bodies
  (assert= (if-get col v [:c :a] "true-called" "false-called") "false-called"))
