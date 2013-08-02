(ns gpai.lang.arith)

(defn _abs_
  [x]
  (when-not (nil? x)
    (if (neg? x) (- x) x)))

(defn _+_
  [x y]
  (when-not (or (nil? x) (nil? y))
    (+ x y)))

(defn _*_
  [x y]
  (when-not (or (nil? x) (nil? y))
    (* x y)))

(defn _-_
  [x y]
  (when-not (or (nil? x) (nil? y))
    (- x y)))

(defn _div_
  [x y]
  (when-not (or (nil? x) (nil? y))
    (if (< (_abs_ y) 1e-6)
      1
      (/ x y))))

(defn _quot_
  [x y]
  (when-not (or (nil? x) (nil? y))
    (if (< (_abs_ y) 1e-6)
      1
      (quot x y))))

(defn _mod_
  [x y]
  (when-not (or (nil? x) (nil? y))
    (if (< (_abs_ y) 1e-6)
      1
      (mod x y))))

(defn _min_
  [x y]
  (when-not (or (nil? x) (nil? y))
    (min x y)))

(defn _max_
  [x y]
  (when-not (or (nil? x) (nil? y))
    (max x y)))

(defn _if<_
  [x y a b]
  (if (< (or x 0) (or y 0)) a b))

(defn _if=_
  [x y a b]
  (if (== (or x 0) (or y 0)) a b))

(def funcset-real
  "Function symbols for real arithmetic."
  `#{_abs_
     _+_
     _-_
     _*_
     _div_
     _mod_
     _min_
     _max_
     _if<_})

(def funcset-int
  "Function symbols for integer arithmetic."
  (into (disj funcset-real `_div_)
        `#{_quot_
           _if=_}))

