(defn assert= [l r &optional msg] (assert (= l r) msg))

(defn assert-resource [tmp res-name res-type &rest tests]
  (unless tmp
    (raise (Exception "template is null")))
  
  (unless (in "Resources" tmp)
    (raise (Exception "missing 'Resources' section in template")))
  
  (setv res (get tmp "Resources" res-name))
  (assert= (get res "Type") res-type)

  (setv props (get res "Properties"))
  (for [t tests] (t res-name props)))

(defn p= [lookup val]
  (unless (coll? lookup)
    (setv lookup [lookup]))
  (fn [res-name props]
    (assert= (apply get (+ [props] lookup)) val
             (.join "." (+ [res-name] lookup)))))
