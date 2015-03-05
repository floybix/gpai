(ns org.nfrac.gpai.lang-logic
  "GP functions working with boolean data.")

(def lang
  `[[_and_ 2]
    [_or_ 2]
    [not 2]
    [_xor_ 2]
    [_if_ 3]])

(defn _and_
  [x y]
  (and x y))

(defn _or_
  [x y]
  (or x y))

(defn _xor_
  [x y]
  (and (or x y)
       (not (and x y))))

(defn _nand_
  [x y]
  (not (and x y)))

(defn _nor_
  [x y]
  (not (or x y)))

(defn _if_
  [x a b]
  (if x a b))
