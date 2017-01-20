(ns org.nfrac.gpai.lang-integer
  "GP functions working with integral (long) data.
   Division by zero is handled by returning 1.
   They will fail on a nil input.")

(def lang
  "Functions to work with integral (long) inputs. They will work with
   general clojure number inputs, but go faster with primitive longs.
   A vector of [symbol function-arity] tuples."
  `[[abs 1]
    [+ 2]
    [* 2]
    [- 2]
    [_quot_ 2]
    [_mod_ 2]
    [min 2]
    [max 2]
    [if< 4]
    [if= 4]])


(defn abs
  ^long [^long x]
  (Math/abs x))

(defn _+_
  ^long [^long x ^long y]
  (unchecked-add (long x) (long y)))

(defn _*_
  ^long [^long x ^long y]
  (unchecked-multiply (long x) (long y)))

(defn _-_
  ^long [^long x ^long y]
  (unchecked-subtract (long x) (long y)))

(defn _quot_
  ^long [^long x ^long y]
  (if (= y 0)
    1
    (quot x y)))

(defn _mod_
  ^long [^long x ^long y]
  (if (= y 0)
    1
    (mod x y)))

(defmacro if<
  [x y a b]
  `(if (< ~x ~y) ~a ~b))

(defmacro if=
  [x y a b]
  `(if (= ~x ~y) ~a ~b))
