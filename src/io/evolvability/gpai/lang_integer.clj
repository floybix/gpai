(ns io.evolvability.gpai.lang-integer)

(defn _abs_
  [x]
  (if (neg? x) (unchecked-negate (long x)) x))

(defn _+_
  [x y]
  (unchecked-add (long x) (long y)))

(defn _*_
  [x y]
  (unchecked-multiply (long x) (long y)))

(defn _-_
  [x y]
  (unchecked-subtract (long x) (long y)))

(defn _quot_
  [x y]
  (if (zero? y)
    1
    (quot x y)))

(defn _mod_
  [x y]
  (if (zero? y)
    1
    (mod x y)))

(defn _min_
  [x y]
  (min x y))

(defn _max_
  [x y]
  (max x y))

(defn _if<_
  [x y a b]
  (if (< x y) a b))

(defn _if=_
  [x y a b]
  (if (== x y) a b))

(def funcset
  "Function symbols for integer arithmetic."
  `#{_abs_
     _+_
     _-_
     _*_
     _quot_
     _mod_
     _min_
     _max_
     _if<_
     _if=_})
