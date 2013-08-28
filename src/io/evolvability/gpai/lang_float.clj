(ns io.evolvability.gpai.lang-float
  "GP functions working with floating point (double) data.
   Division by zero is handled by returning 1.0.
   They will fail on a nil input.")

(def lang
  "Functions to work with floating point (double) inputs. They will
   work with general clojure number inputs, but go faster with
   primitive doubles. A vector of [symbol function-arity] tuples."
  `[[_abs_ 1]
    [_+_ 2]
    [_*_ 2]
    [_-_ 2]
    [_div_ 2]
    [_mod_ 2]
    [min 2]
    [max 2]
    [_if<_ 4]
    ])

(defn _abs_
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
  (if (< (_abs_ y) 1e-6)
    1.0
    (/ x y)))

(defn _mod_
  ^double [^double x ^double y]
  (if (< (_abs_ y) 1e-6)
    1.0
    (mod x y)))

(defn _if<_
  ^double [^double x ^double y ^double a ^double b]
  (if (< x y) a b))
