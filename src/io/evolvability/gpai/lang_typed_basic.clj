(ns io.evolvability.gpai.lang-typed-basic
  "Typed GP functions working with numeric and boolean data.")

(def lang-boolean
  [[`not [Boolean, Boolean]]
   [`and [Boolean, Boolean Boolean]]
   [`or [Boolean, Boolean Boolean]]])

(def lang-long
  "Operators for integer (long) data. Supports arbitrary precision."
  [[`abs [Long, Long]]
   [`inc' [Long, Long]]
   [`+' [Long, Long Long]]
   [`-' [Long, Long Long]]
   [`*' [Long, Long Long]]
   [`_quot_ [Long, Long Long]]
   [`_mod_ [Long, Long Long]]
   [`min [Long, Long Long]]
   [`max [Long, Long Long]]
   [`< [Boolean, Long Long]]
   [`= [Boolean, Long Long]]
   [`if [Long, Boolean Long Long]]])

(def lang-long-prim
  "Operators for primitive integer (long) data. If used with strictly
   primitive longs (and if compiled with unchecked math), will allow
   overflows silently. Otherwise throws ArithmeticException."
  [[`abs [Long, Long]]
   [`inc [Long, Long]]
   [`+ [Long, Long Long]]
   [`- [Long, Long Long]]
   [`* [Long, Long Long]]
   [`_quot_ [Long, Long Long]]
   [`_mod_ [Long, Long Long]]
   [`min [Long, Long Long]]
   [`max [Long, Long Long]]
   [`< [Boolean, Long Long]]
   [`= [Boolean, Long Long]]
   [`if [Long, Boolean Long Long]]])

(def lang-double
  "Operators for floating point (double) data."
  [[`abs [Double, Double]]
   [`inc [Double, Double]]
   [`+ [Double, Double Double]]
   [`- [Double, Double Double]]
   [`* [Double, Double Double]]
   [`_div_ [Double, Double Double]]
   [`_dmod_ [Double, Double Double]]
   [`min [Double, Double Double]]
   [`max [Double, Double Double]]
   [`< [Boolean, Double Double]]
   [`if [Double, Boolean Double Double]]])

(defmacro abs
  "Absolute value of a number. This is a macro rather than function to
   avoid autoboxing."
  [x]
  `(if (pos? ~x) ~x (- ~x)))

(defn _quot_
  "quot, returns 1 if y is zero."
  ^long [^long x ^long y]
  (if (= y 0)
    1
    (quot x y)))

(defn _mod_
  "mod, returns 1 if y is zero."
  ^long [^long x ^long y]
  (if (= y 0)
    1
    (mod x y)))

(defn _div_
  "/, returns 1.0 if y is within 1e-6 of zero."
  ^double [^double x ^double y]
  (if (< (abs y) 1e-6)
    1.0
    (/ x y)))

(defn _dmod_
  "mod, returns 1.0 if y is within 1e-6 of zero."
  ^double [^double x ^double y]
  (if (< (abs y) 1e-6)
    1.0
    (mod x y)))
