(ns io.evolvability.gpai.lang-logic)

(defn _and_
  [x y]
  (and x y))

(defn _or_
  [x y]
  (or x y))

(defn _not_
  [x]
  (not x))

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

(def funcset-basic
  `#{_and_
     _or_
     _not_
     _xor_
     _if_})
