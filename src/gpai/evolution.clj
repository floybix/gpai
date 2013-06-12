(ns gpai.evolution
  (:use gpai.core)
  (:require [clojure.java.io :as io]))

(defn fullymixed-regeneration-fn
  "Returns a regeneration function taking a fitness-evaluated population and
   returning a new generation population. This involves both selection and
   variation. It applies the given operators:
   * the top :select-n individuals by fitness are selected.
   * A number :elitism of highest-fitness individuals are preserved.
   * mutate function is applied to selected individuals to generate a
     proportion :mutation-prob of the next population.
   * crossover function is applied to selected pairs to generate the rest."
  [mutate crossover
   & {:keys [select-n elitism mutation-prob]
      :or {select-n 3, elitism 1, mutation-prob 0.2}}]
  (fn [pop]
    (let [n (count pop)
          n-mutate (long (* n mutation-prob))
          sortd (reverse (sort-by (comp :fitness meta) pop))
          parents (take select-n sortd)
          new-mutant #(mutate (rand-nth parents))
          new-child #(crossover (rand-nth parents)
                                (rand-nth parents))]
      (concat (take elitism parents)
              (repeatedly n-mutate new-mutant)
              (repeatedly (- n n-mutate elitism) new-child)))))

(defn summarise-keep-best
  [pop]
  (let [sortd (sort-by (comp :fitness meta) pop)
        fso (map (comp :fitness meta) sortd)
        fit-max (last fso)
        fit-min (first fso)
        fit-med (median fso)
        best (last sortd)]
    {:fit-max fit-max
     :fit-min fit-min
     :fit-med fit-med
     :best best}))

(defn print-progress
  [pop i]
  (let [fs (map (comp :fitness meta) pop)
        fso (sort fs)
        fit-max (last fso)
        fit-min (first fso)
        fit-med (median fso)]
    (println (format "Gen %d: fitnesses [ %+8.3f  %+8.3f  %+8.3f ]"
                     i (double fit-min) (double fit-med) (double fit-max)))))

(defn evolve
  "Generic evolution function where fitness is a function of one individual.
   * init-pop is the initial population as a sequence.
   * fitness function is applied to an individual, returns numeric where
     larger numbers represent higher fitness. Consider using (memoize fitness).
   * regeneration function is applied to the whole population sequence where
     fitness values for each individual are in meta :fitness. It returns a
     corresponding new population with new individuals that are typically
     derived by mutation or crossover.
   * summarise function is applied to the fitness-evaluated population every
     generation and the result appended to a history vector which is returned.
   * There are `:n-gens` generations.
   * Every `:snapshot-secs` seconds (5 mins) the state is written out to a file
     `:snapshot-out` defaulting to \"snapshot-out.edn\". The value is a map with
     same structure as the final return value - see below.
   * The function `:progress` is called every generation with 2 arguments, the
     fitness-evaluated population and the generation number. The default prints
     fitness summaries.
   * `:map-fn` is used to map fitness calculations over the population. It can
     be set to `pmap` if the fitness function is thread-safe.
   Returns a map with same structure as the snapshots:
   * :pop final population,
   * :history the history vector,
   * :i generation number.
   "
  [init-pop fitness regenerate summarise
   & {:keys [n-gens snapshot-secs snapshot-out progress progress-every map-fn]
      :or {n-gens 100
           snapshot-secs (* 5 60)
           snapshot-out "snapshot-out.edn"
           progress #'print-progress
           progress-every 1
           map-fn #'map}}]
  (let [eval-fitness (fn [x] (vary-meta x assoc :fitness (fitness x)))
        t0 (System/nanoTime)]
    (loop [pop init-pop
           history []
           i 1
           snapshot-t t0]
      (let [t (System/nanoTime)
            newsecs (/ (- t snapshot-t) 1e9)
            do-snapshot? (and snapshot-secs
                              (> newsecs snapshot-secs))]
        (when do-snapshot?
          (future
            (with-open [w (io/writer snapshot-out)]
              (binding [*out* w]
                (pr {:pop pop :history history :i i})
                (flush)))))
        (if (<= i n-gens)
          (let [evald-pop (map-fn eval-fitness pop)]
            (when (or (== i 1)
                      (zero? (mod i progress-every)))
              (progress evald-pop i))
            (recur (regenerate evald-pop)
                   (conj history (summarise evald-pop))
                   (inc i)
                   (if do-snapshot? t snapshot-t)))
          ;; finished
          {:pop pop :history history :i i})))))
