(ns io.evolvability.gpai.utils
  (:require [clojure.java.io :as io]))

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

(defn snapshot-to-file-fn
  "Returns a function usable as `:progress!` in `evolution`, taking 3
   args. After an elapsed time of `every-mins` minutes it writes a
   snapshot to file `file` . The data written is a map with
   keys :popn, :history and :n-gens."
  [file every-mins]
  (let [last-snap (atom (System/currentTimeMillis))]
    (fn [i popn history]
      (let [t (System/currentTimeMillis)
            newmins (/ (- t @last-snap) 1000 60)
            do-snapshot? (>= newmins every-mins)]
        (when do-snapshot?
          (future
            (with-open [w (io/writer file)]
              (binding [*out* w
                        *print-meta* true]
                (pr {:popn popn :history history :n-gens i})
                (flush))))
          (reset! last-snap t))))))
