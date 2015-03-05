(ns org.nfrac.gpai.utils
  (:require [clojure.java.io :as io]))

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

(defn sign
  [x]
  (if (zero? x) 0
      (if (pos? x) 1 -1)))

(defn ts-peaks
  "Takes a numeric time series and returns a sequence corresponding to
   local peaks. Each item is a map like
   `{:start i, :end i+d, :duration d, :value v}`."
  [xs]
  ;; calculate forward differences.
  ;; tail is marked as decrease: this allows us to include a final peak.
  (let [diffs (concat (map - (next xs) xs) [-1])
        i-diff-xs (map vector (range (count xs)) diffs xs)
        slopes (partition-by (comp sign second) i-diff-xs)
        peaks (map (fn [[p c n]]
                     (let [[_ pd _] (first p)
                           [i cd cv] (first c)
                           [ni nd _] (first n)]
                       (when (and (pos? pd)
                                  (or (neg? cd)
                                      (and (zero? cd)
                                           (neg? nd))))
                         ;; peak
                         (let [end (if (neg? cd) i ni)]
                           {:start i
                            :end end
                            :duration (inc (- end i))
                            :value cv}))))
                   (partition 3 1 slopes))]
    (remove nil? peaks)))

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
