(ns gpai.lang.arith
  (:use gpai.lang.core))

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

(defn if<
  [x y a b]
  (if (< (or x 0) (or y 0)) a b))

(def funcset
  "Function symbols in this namespace."
  `#{_abs_
     _+_
     _-_
     _*_
     _div_
     _mod_
     _min_
     if<})
