(ns org.nfrac.gpai.evolution
  (:require [org.nfrac.gpai.utils :as utils]
            [clojure.data.generators :as gen]))

(defn get-fitness
  "Returns the previously calculated fitness value stored in metadata
   key :org.nfrac.gpai.evolution/fitness, or nil."
  [x]
  (::fitness (meta x)))

(defn get-fitness-0
  [x]
  (or (get-fitness x) 0))

(defn tag-fitness
  [x fitness-val]
  (vary-meta x assoc ::fitness fitness-val))

(defn tournaments-fn
  "Returns a regenerate function taking a fitness-evaluated population
   and deriving the next generation population.

   In tournament selection, the population is filled by a series of
   tournaments each with `size` randomly-selected contestants. The two
   with highest fitness are passed through the `crossover` function
   and then `mutate` to produce a new individual. Also, a number
   `:elitism` of highest-fitness individuals are passed through
   unchanged.

   Individuals with equal fitness are selected between randomly. This
   allows neutral mutations to accumulate over time.

   For no crossover pass `crossover` as nil. For crossover with some
   probability, test for it inside your `crossover` function."
  [size mutate crossover
   & {:keys [elitism] :or {elitism 0}}]
  {:pre [(>= size 2) (>= elitism 0)]}
  (fn [xs]
    (let [n (count xs)
          crossover (or crossover (fn [x _] x))
          ;; shuffle to avoid deterministic elitism with equal fitness
          fitsort (fn [xs] (sort-by (comp - get-fitness) (gen/shuffle xs)))
          new-child (fn [] (let [ps (-> (repeatedly size #(gen/rand-nth xs))
                                       fitsort)]
                            (-> (first ps)
                                (crossover (second ps))
                                (mutate))))]
      (concat (take elitism (fitsort xs))
              (repeatedly (- n elitism) new-child)))))

(defn negative-selection-fn
  "Returns a regenerate function taking a fitness-evaluated population
   and deriving the next generation population.

   In negative selection, the top `select-n` individuals by fitness
   are selected, i.e. the remainder are discarded completely. The
   population is filled by randomly choosing pairs of individuals,
   applying the `crossover` operator and finally the `mutate`
   operator. Also, a number `:elitism` of highest-fitness individuals
   are passed through unchanged.

   Individuals with equal fitness are selected between randomly. This
   allows neutral mutations to accumulate over time.

   For no crossover pass `crossover` as nil. For crossover with some
   probability, test for it inside your `crossover` function."
  [select-n mutate crossover
   & {:keys [elitism] :or {elitism 0}}]
  {:pre [(>= select-n 1) (>= elitism 0) (>= select-n elitism)]}
  (fn [xs]
    {:pre [(>= (count xs) select-n)]}
    (let [n (count xs)
          crossover (or crossover (fn [x _] x))
          ;; shuffle to avoid deterministic elitism with equal fitness
          fitsort (fn [xs] (sort-by (comp - get-fitness) (gen/shuffle xs)))
          pool (take select-n (fitsort xs))
          new-child #(-> (gen/rand-nth pool)
                         (crossover (gen/rand-nth pool))
                         (mutate))]
      (concat (take elitism pool)
              (repeatedly (- n elitism) new-child)))))

(defn basic-distil
  "Takes a fitness-evaluated population collection and returns a map
   with the generation champion in key :best, as well as fitness
   statistics in keys :fit-max :fit-min :fit-med."
  [xs]
  (let [sortd (sort-by get-fitness-0 xs)
        fso (map get-fitness-0 sortd)
        fit-hi (last fso)
        fit-lo (first fso)
        fit-med (utils/median fso)
        best (last sortd)]
    {:fit-max fit-hi
     :fit-min fit-lo
     :fit-med fit-med
     :best best}))

(defn print-progress
  [i xs _]
  (let [fso (sort (map get-fitness-0 xs))
        fit-hi (last fso)
        fit-lo (first fso)
        fit-med (utils/median fso)]
    (println (format "Gen %d: fitnesses [ %+8.3f  %+8.3f  %+8.3f ]"
                     i (double fit-lo) (double fit-med) (double fit-hi)))))

(defn evolve-discrete
  "General evolution function with discrete generations.

   This function works with collections of individuals. Individuals
   must support metadata (all Clojure types do), since fitness values
   are stored in metadata (with key ::fitness). This is approprate
   since metadata retains equality comparisons between individuals.

   Having the fitness evaluation function apply to the whole
   population, and depend on the previous generation and evolution
   history, allows for coevolution schemes including selection of
   opponents.

   * init-popn is the initial population collection.
   * eval-popn-fitness function is called with 3 arguments:
     1. the current population collection, having been regenerated,
        generally without fitness values;
     2. the previous, fitness-evaluated population, usually nil on the
        first generation (see option `:prev-popn`);
     3. the history vector, accumulated results from distil function.
     It must return the same individuals with fitness values in
     metadata key ::fitness. (Allowing the sequence order to change,
     for convenience.) Larger numbers represent higher fitness. The
     fitness-evaluated population will be passed on to the following
     two functions.
   * regenerate function takes the fitness-evaluated population
     collection. It returns a new population with new individuals that
     are typically derived by mutation or crossover. The new
     population need not be the same size.

   Options map keys:

   * The evolution returns after `:n-gens` generations (default 100)
     or when fitness reaches `:target` (default infinity).
   * distil function takes the fitness-evaluated population collection
     and produces some summary. By convention the generation champion
     individual is included in key :best. The result is appended to
     the history vector. The history vector is passed to
     `eval-popn-fitness` and is eventually returned by this function.
   * The function `:progress!` is called every generation, after
     `eval-popn-fitness` is called, with 3 arguments:
     1. the generation number;
     2. the fitness-evaluated population from the current generation;
     3. the history vector, including the current generation.
     The default prints fitness summaries. Another use would be to
     write out the current state to a backup file.
     To call `:progress!` only every nth generation the option
     `:progress-every` can be set, but it will always be called on the
     first and last generations.
   * The fitness-evaluated population argument to `eval-popn-fitness`
     for the first iteration can be given in key `:prev-popn`. This
     allows an evolution run to be continued seamlessly from a
     snapshot.

   Returns a map with keys

   * `:popn` final population with fitness values
   * `:history` the history vector
   * `:n-gens` number of generations run."
  [init-popn eval-popn-fitness regenerate
   {:keys [n-gens target distil progress! progress-every prev-popn]
    :or {n-gens 100
         target Double/POSITIVE_INFINITY
         distil #'basic-distil
         progress! #'print-progress
         progress-every 1
         prev-popn nil}}]
  (loop [popn (seq init-popn)
         prev-popn (seq prev-popn)
         history []
         i 1]
    (let [evald-popn (eval-popn-fitness popn prev-popn history)
          newhistory (conj history (distil evald-popn))
          maxfit (->> (map get-fitness-0 evald-popn)
                      (apply max))]
      (when (or (== i 1)
                (== i n-gens)
                (>= maxfit target)
                (zero? (mod i progress-every)))
        (progress! i evald-popn newhistory))
      (if (or (>= maxfit target)
              (>= i n-gens))
        {:popn evald-popn :history newhistory :n-gens i}
        (recur (regenerate evald-popn)
               evald-popn
               newhistory
               (inc i))))))

(defn simple-evolve
  "High-level evolution function where fitness is simply a function of
   any one individual.

   * init-popn is the initial population collection.
   * fitness function is applied to an individual, returning numeric
     where larger numbers represent higher fitness. Consider
     using (memoize fitness).
   * option `:map-fn` is used to map fitness calculations over the
     population. It can be set to `pmap` if the fitness function is
     thread-safe.
   * other options are passed through directly to `evolve-discrete`."
  [init-popn fitness regenerate
   {:as options
    :keys [map-fn]
    :or {map-fn #'map}}]
  (let [eval-fitness (fn [x] (tag-fitness x (fitness x)))
        map-fitness (fn [xs _ _] (map-fn eval-fitness xs))]
    (evolve-discrete init-popn map-fitness regenerate options)))
