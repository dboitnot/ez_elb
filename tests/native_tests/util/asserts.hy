(defn assert= [l r] (assert (= l r)))

(defn assert-resource [tmp res-name res-type]
  (unless tmp
    (raise (Exception "template is null")))
  
  (unless (in "Resources" tmp)
    (raise (Exception "missing 'Resources' section in template")))
  
  (setv res (get tmp "Resources" res-name))
  (assert= (get res "Type") res-type))
