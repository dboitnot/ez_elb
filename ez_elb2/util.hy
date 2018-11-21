(import inspect)

(defn dict-keys-kw->str [d]
  "returns a new dictionary with Hy keyword keys converted to strings"
  (dict (map (fn [p] [(name (get p 0)) (get p 1)]) (.items d))))

(defn apply-kw [f pargs kws]
  
  "apply the function f with the keyword args kws specified as Hy
  keywords with positional args pargs"
  
  (apply f pargs (dict-keys-kw->str kws)))

(defn pop-head [col &optional [n 1]]
  
  "Removes and returns the first n (default 1) values off the HEAD of
  a collection, always returns a list."
  
  (setv ret (cut col 0 n))
  (del (cut col 0 n))  
  ret)

(defn pop-head! [col]
  "Removes and returns the first item from a collection"
  (get (pop-head col) 0))

(defn list-pairs->tag-list [l]
  "returns a list of maps suitable for specifying AWS tags from a list
  of paired strings"

  (list (map (fn [p] {"Key" (get p 0) "Value" (get p 1)}) (partition l))))

(defn arity [f]
  "returns the arity of the given function"
  (len (get (inspect.getargspec f) 0)))

(defn map-dict [kf vf d]
  
  "returns a new dictionary from d with it's keys mapped through kf
  and it's values mapped through vf"

  (dict (map (fn [k] [(kf k) (vf (get d k))]) d)))

(defmacro if-get [coll v lookup if-true &optional [if-false '(do)]]

  "If lookup can be found in coll then v is assigned it's value and
  if-true is called. Otherwise if-false is called."

  (if-not (coll? lookup)
          (setv lookup [lookup]))

  `(try
     (setv ~v (apply (partial get ~coll) ~lookup))
     ~if-true
     (except [KeyError] ~if-false)))
