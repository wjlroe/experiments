(defn indexed [coll] (map vector (iterate inc 0) coll))
(defn index-filter [pred coll]
  (when pred
    (for [[idx elt] (indexed coll) :when (pred elt)] idx)))