(ns gpai.utils)

(defn arity
  "Looks up the (first) arity of a function symbol according to
   meta :arglists. Returns nil for anything other than a symbol."
  [f-sym]
  (when (symbol? f-sym)
    (-> f-sym
        resolve
        meta
        :arglists
        first
        count)))

(defn nil-contagion
  "Gives a function like f but that immediately returns nil when any of
  its arguments are nil."
  [f]
  (fn
    ([a] (when-not (nil? a) (f a)))
    ([a b] (when-not (or (nil? a) (nil? b)) (f a b)))
    ([a b & cs] (when-not (some nil? (concat [a b] cs)) (apply f a b cs)))))

(defn median
  "Median value of a sorted sequence."
  [x]
  (nth x (quot (dec (count x)) 2)))
