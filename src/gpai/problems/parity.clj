(ns gpai.problems.parity)

(defn actual-fn
  [& args]
  (even? (count (filter identity args))))

(defn gen-inputs
  "Generate a list of input cases for the given parity order."
  [n]
  (for [i (range (Math/pow 2 n))]
    (mapv (partial bit-test i) (range n))))

(defn fitness-fn
  [inputs f]
  (/ (count (filter #(= (apply f %)
                        (apply actual-fn %))
                    inputs))
     (count inputs)))
