(ns gpai.core)

(defn nan? [x]
  (and (number? x) (Double/isNaN x)))

(defn median
  "Median value of a sorted sequence."
  [x]
  (nth x (quot (dec (count x)) 2)))
