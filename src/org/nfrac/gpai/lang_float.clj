(ns org.nfrac.gpai.lang-float
  "GP functions working with floating point (double) data.
   Division by zero is handled by returning 1.0.
   They will fail on a nil input.")

(def lang
  "Functions to work with floating point (double) inputs. They will
   work with general clojure number inputs, but go faster with
   primitive doubles. A vector of [symbol function-arity] tuples."
  `[[abs 1]
    [+ 2]
    [* 2]
    [- 2]
    [_div_ 2]
    [_mod_ 2]
    [min 2]
    [max 2]
    [if< 4]])


(defn abs
  ^double [^double x]
  (Math/abs x))

(defn _+_
  ^double [^double x ^double y]
  (+ x y))

(defn _*_
  ^double [^double x ^double y]
  (* x y))

(defn _-_
  ^double [^double x ^double y]
  (- x y))

(defn _div_
  ^double [^double x ^double y]
  (if (< (abs y) 1e-6)
    1.0
    (/ x y)))

(defn _mod_
  ^double [^double x ^double y]
  (if (< (abs y) 1e-6)
    1.0
    (mod x y)))

(defmacro if<
  [x y a b]
  `(if (< ~x ~y) ~a ~b))
