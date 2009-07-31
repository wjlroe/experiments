(defmulti encounter (fn [x y] [(:Species x) (:Species y)]))

(defmethod encounter [:Bunny :Lion] [b l] :run-away)
(defmethod encounter [:Lion :Bunny] [l b] :eat)
(defmethod encounter [:Lion :Lion] [l1 l2] :fight)
(defmethod encounter [:Bunny :Bunny] [b1 b2] :mate)

(def b1 {:Species :Bunny :Other :stuff})
(def b2 {:Species :Bunny :Other :stuff})
(def l1 {:Species :Lion :Other :stuff})
(def l2 {:Species :Lion :Other :stuff})

(encounter b1 l1)
(encounter l2 b2)
(encounter l1 l2)
(encounter b2 b1)

(defn my-filter [func lst]
  (pr "List being filtered: " lst)
  (filter func lst))

(defn quicksort
  [[pivot & rest]]
     (if (nil? rest) (when (not (nil? pivot)) (list pivot))
	 (concat 
	  (quicksort (filter (fn [x] (< x pivot)) rest)) 
	  (list pivot) 
	  (quicksort (filter (fn [x] (>= x pivot)) rest)))))

(defn addone [numb]
  (+ numb 1))

(defn take-first
  ([[fst & rest]] fst))

(defn take-last
  ([[fst & rest]] rest))

(defn sum-int [a b]
  (if (> a b) 0
      (+ a 
	 (sum-int (+ 1 a) b))))

(defn square-int [a b]
  (if (> a b)
    0
    (+ (* a a)
       (square-int (+ 1 a) b))))

(take 4
      (let [numbers
      (take 6 #(fn 