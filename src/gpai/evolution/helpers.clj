(ns gpai.evolution.helpers
  (:require [clojure.java.io :as io]))

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
