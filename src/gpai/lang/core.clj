(ns gpai.lang.core)

(defn arity
  [f-sym]
  (-> f-sym
      resolve
      meta
      :arglists
      first
      count))

(defn nil-contagion
  "Gives a function like f but that immediately returns nil when any of
  its arguments are nil."
  [f]
  (fn
    ([a] (when-not (nil? a) (f a)))
    ([a b] (when-not (or (nil? a) (nil? b)) (f a b)))
    ([a b & cs] (when-not (some nil? (concat [a b] cs)) (apply f a b cs)))))

(defn fn-from-expr
  "Generate a function from a quoted expression and argument vector.
   We eval in given namespace for required closure over lang functions
   in case any symbols are not fully qualified."
  ([args expr]
     (eval `(fn ~args ~expr)))
  ([lang-ns args expr]
     (binding [*ns* lang-ns]
       (eval `(fn ~args ~expr)))))
