(ns gpai.evolution
  (:require [gpai.utils :as utils]
            [clojure.java.io :as io]))

(defn fullymixed-regeneration-fn
  "Returns a regeneration function taking a fitness-evaluated
   population and returning a new generation population. This involves
   both selection and variation. It applies the given operators:
   * the top :select-n individuals by fitness are selected.
   * a number :elitism of highest-fitness individuals are preserved.
   * mutate function is applied to selected individuals to generate a
     proportion :mutation-prob of the next population.
   * crossover function is applied to selected pairs to generate the
     rest."
  [mutate crossover
   & {:keys [select-n elitism mutation-prob]
      :or {select-n 3, elitism 1, mutation-prob 0.2}}]
  (fn [xs]
    (let [n (count xs)
          n-mutate (long (* (- n elitism) mutation-prob))
          sortd (reverse (sort-by (comp :fitness meta) xs))
          parents (take select-n sortd)
          new-mutant #(mutate (rand-nth parents))
          new-child #(crossover (rand-nth parents)
                                (rand-nth parents))]
      (concat (take elitism parents)
              (repeatedly n-mutate new-mutant)
              (repeatedly (- n elitism n-mutate) new-child)))))

(defn summarise-keep-best
  [xs]
  (let [sortd (sort-by (comp :fitness meta) xs)
        fso (map (comp :fitness meta) sortd)
        fit-max (last fso)
        fit-min (first fso)
        fit-med (utils/median fso)
        best (last sortd)]
    {:fit-max fit-max
     :fit-min fit-min
     :fit-med fit-med
     :best best}))

(defn print-progress
  [xs i]
  (let [fs (map (comp :fitness meta) xs)
        fso (sort fs)
        fit-max (last fso)
        fit-min (first fso)
        fit-med (utils/median fso)]
    (println (format "Gen %d: fitnesses [ %+8.3f  %+8.3f  %+8.3f ]"
                     i (double fit-min) (double fit-med) (double fit-max)))))

(defn evolve-general
  "General evolution function. Larger numbers represent higher fitness.
   * init-popn is the initial population as a sequence.
   * eval-popn-fitness function is called with 3 arguments:
     * the whole population sequence. It must return the same
       population with numeric fitness values in each individual's
       metadata key `:fitness`.
     * the fitness-evaluated population from the previous
       generation (nil on the first generation).
     * the history vector (from :summarise option).
   * regeneration function is applied to the whole population sequence
     where fitness values for each individual are in meta :fitness. It
     returns a corresponding new population with new individuals that
     are typically derived by mutation or crossover.

   Options map keys:
   * The evolution returns after `:n-gens` generations (default 100)
     or when fitness reaches `:target` (default infinity).
   * The `:summarise` function is applied to the fitness-evaluated
     population every generation and the result appended to a history
     vector which is returned. The default is `summarise-keep-best`
     which stores the generation champion in key :best, as well as
     fitness statistics.
   * Every `:snapshot-secs` seconds (5 mins) the state is written out
     to a file `:snapshot-out` defaulting to \"snapshot-out.edn\". The
     value is a map with same structure as the final return value -
     see below.
   * The function `:progress` is called every generation with 2
     arguments, the fitness-evaluated population and the generation
     number. The default prints fitness summaries.

   Returns a map with same structure as the snapshots:
   * :popn final population with fitness values
   * :history the history vector
   * :i generation number."
  [init-popn eval-popn-fitness regenerate
   {:as options
    :keys [target n-gens summarise snapshot-secs snapshot-out
           progress progress-every]
    :or {target Double/POSITIVE_INFINITY
         n-gens 100
         snapshot-secs (* 5 60)
         snapshot-out "snapshot-out.edn"
         summarise #'summarise-keep-best
         progress #'print-progress
         progress-every 1}}]
  (let [t0 (System/currentTimeMillis)]
    (loop [popn init-popn
           prev-popn nil
           history []
           i 1
           snapshot-t t0]
      (let [t (System/currentTimeMillis)
            newsecs (/ (- t snapshot-t) 1000)
            do-snapshot? (and snapshot-secs
                              (> newsecs snapshot-secs))]
        (when do-snapshot?
          (future
            (with-open [w (io/writer snapshot-out)]
              (binding [*out* w
                        *print-meta* true]
                (pr {:popn prev-popn :history history :i i})
                (flush)))))
        (if (<= i n-gens)
          (let [evald-popn (eval-popn-fitness popn prev-popn history)
                newhistory (conj history (summarise evald-popn))
                maxfit (reduce max (map (comp :fitness meta) evald-popn))]
            (when (or (== i 1)
                      (zero? (mod i progress-every))
                      (>= maxfit target))
              (progress evald-popn i))
            (if (>= maxfit target)
              ;; reached target fitness
              {:popn evald-popn :history newhistory :i i}
              (recur (regenerate evald-popn)
                     evald-popn
                     newhistory
                     (inc i)
                     (if do-snapshot? t snapshot-t))))
          ;; finished
          {:popn prev-popn :history history :i i})))))

(defn evolve
  "High-level evolution function where fitness is a function of one
   individual.
   * init-popn is the initial population as a sequence.
   * fitness function is applied to an individual, returning numeric
     where larger numbers represent higher fitness. Consider
     using (memoize fitness).
   * `:map-fn` is used to map fitness calculations over the
     population. It can be set to `pmap` if the fitness function is
     thread-safe.
   * other arguments are passed through directly to `evolve-general`."
  [init-popn fitness regenerate
   {:as options
    :keys [map-fn]
    :or {map-fn #'map}}]
  (let [eval-fitness (fn [x] (vary-meta x assoc :fitness (fitness x)))
        map-fitness (fn [xs _ _] (map-fn eval-fitness xs))]
    (evolve-general init-popn map-fitness regenerate options)))
