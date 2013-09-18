(ns io.evolvability.gpai.lang-typed-basic
  "GP functions working with combinations of atomic numeric and boolean types.
   ")

;; TODO: type inference for general signatures with specific inputs?
;; (Number -> Double)

(def lang-boolean-typed 
  "Type declarations to work with ccgp."
  [[`not [Boolean, Boolean]]
   [`and [Boolean, Boolean Boolean]]
   [`or [Boolean, Boolean Boolean]]
   [`if [Boolean, Boolean Boolean Boolean]]
   ])

(def lang-number-typed
  "Type declarations to work with ccgp."
  [[`abs [Number, Number]]
   [`+ [Number, Number Number]]
   [`- [Number, Number Number]]
   [`* [Number, Number Number]]
   [`_div_ [Number, Number Number]]
   [`_mod_ [Number, Number Number]]
   [`min [Number, Number Number]]
   [`max [Number, Number Number]]
   [`< [Boolean, Number Number]]
   [`= [Boolean, Number Number]]
   [`if [Number, Boolean Number Number]]
   ])

(defn abs
  [x]
  (if (pos? x) x (- x)))

(defn _div_
  [x y]
  (if (integer? x)
    (if (= y 0)
      1
      (quot x y))
    (if (< -1e-6 y 1e-6)
      1.0
      (/ x y))))

(defn _mod_
  [x y]
  (if (integer? x)
    (if (= y 0)
      1
      (mod x y))
    (if (< -1e-6 y 1e-6)
      1.0
      (mod x y))))
