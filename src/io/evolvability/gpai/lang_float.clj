(ns io.evolvability.gpai.lang-float)

(defn _abs_
  [x]
  (if (neg? x) (- x) x))

(defn _+_
  [x y]
  (+ x y))

(defn _*_
  [x y]
  (* x y))

(defn _-_
  [x y]
  (- x y))

(defn _div_
  [x y]
  (if (< (_abs_ y) 1e-6)
    1.0
    (/ x y)))

(defn _mod_
  [x y]
  (if (< (_abs_ y) 1e-6)
    1.0
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

(def funcset
  "Function symbols for floating point arithmetic."
  `#{_abs_
     _+_
     _-_
     _*_
     _div_
     _mod_
     _min_
     _max_
     _if<_})
