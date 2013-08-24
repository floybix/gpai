(ns io.evolvability.gpai.coevolution
  (:require [io.evolvability.gpai.evolution :as evo]))

(defn stratified-basic-distil
  [xs]
  (let [strata (group-by (comp ::popn meta) xs)]
    (zipmap (keys strata)
            (map evo/basic-distil (vals strata)))))

(defn stratified-print-progress
  [i xs h]
  (let [strata (group-by (comp ::popn meta) xs)]
    (doseq [[id sxs] (sort strata)]
      (print (str id " "))
      (evo/print-progress i sxs h))))

(defn- mean
  [xs]
  (/ (double (reduce + xs))
     (count xs)))

(defn coevolve
  "High-level coevolution function, where fitness is determined by
   one-on-one competitions. As 'host-parasite coevolution', it
   maintains two separate populations (:a and :b); individuals only
   duel opponents from the other population. An individual's fitness
   value is its average over all parasites, since the number of
   parasites can vary over generations.

   * init-popn-a is the initial :a population collection.
   * init-popn-b is the initial :b population collection.
   * fitness function is applied to two individuals in order [a b],
     and returns their respective fitness scores (a vector of 2
     numbers), typically the number of fitness cases won by each.
   * parasites function is used to select a few individuals as worthy
     opponents (based on quality and diversity). It is called with 3
     arguments:
     * the sub-population from which to select representatives. This
       is the previous, fitness-evaluated population, or the initial
       population in the first generation.
     * the history vector.
     * the sub-population identifier, :a or :b.
   * regenerate functions are applied to each fitness-evaluated
     sub-population. They should return a corresponding new population
     with new individuals that are typically derived by mutation or
     crossover.
   Other options are passed on to `evolve-discrete`. The default
   `:distil` is `stratified-basic-distil` and the default `:progress`
   is `stratified-print-progress`."
  ([init-popn-a init-popn-b
    fitness
    parasites-fn
    regenerate
    options]
     (coevolve init-popn-a init-popn-b
               fitness
               parasites-fn parasites-fn
               regenerate regenerate
               options))
  ([init-popn-a init-popn-b
    fitness
    parasites-fn-a parasites-fn-b
    regenerate-a regenerate-b
    options]
     (let [options (merge {:distil #'stratified-basic-distil
                           :progress! #'stratified-print-progress}
                          options)
           ;; we guarantee that fitness fn will be called with order [a b]
           eval-fitness (fn [host parasites f]
                          (let [fit (mean (map f parasites))]
                            (evo/tag-fitness host fit)))
           eval-fitness-a (fn [a-host parasites]
                            (eval-fitness a-host parasites
                                          #(first (fitness a-host %))))
           eval-fitness-b (fn [b-host parasites]
                            (eval-fitness b-host parasites
                                          #(second (fitness % b-host))))
           brand (fn [id x] (vary-meta x assoc ::popn id))
           brand-all (fn [id xs] (map (partial brand id) xs))
           init-popn (concat (brand-all :a init-popn-a)
                             (brand-all :b init-popn-b))
           eval-popn (fn [xs prev-xs history]
                       (let [strata (group-by (comp ::popn meta) xs)
                             pstrata (group-by (comp ::popn meta) prev-xs)
                             a-paras (parasites-fn-a (or (:a pstrata) (:a strata))
                                                     history :a)
                             b-paras (parasites-fn-b (or (:b pstrata) (:b strata))
                                                     history :b)]
                         (concat (map #(eval-fitness-a % b-paras) (:a strata))
                                 (map #(eval-fitness-b % a-paras) (:b strata)))))
           regenerate (fn [xs]
                        (let [strata (group-by (comp ::popn meta) xs)]
                          (concat (brand-all :a (regenerate-a (:a strata)))
                                  (brand-all :b (regenerate-b (:b strata))))))]
       (evo/evolve-discrete init-popn eval-popn regenerate options))))

(defn basic-parasites-fn
  "Returns a basic parasites selection function that gives the top n
   individuals by fitness from the current population. It only
   considers fitness, not diversity, and not history."
  [n]
  (fn [xs _ _]
    (let [sortd (sort-by evo/get-fitness xs)]
      (take-last n sortd))))

(defn sign
  [x]
  (if (zero? x) 0
      (if (pos? x) 1 -1)))

(defn fill-in-slopes
  "Take a partitioned sequence like
   ((-1 -1) (0 0) (-1) (0) (1) (0) (1 1))
   and fill in groups of 0s which are surrounded by values of same
   sign. So the example above yeilds
   ((-1 -1 -1 -1 -1) (0) (1 1 1 1))."
  [parts]
  (let [innerfilled (map (fn [[p c n]]
                           (if (and (zero? (first c))
                                    (== (sign (first p))
                                        (sign (first n))))
                             (repeat (count c) (first n))
                             c))
                         (partition 3 1 parts))
        filled (concat (take 1 parts)
                       innerfilled
                       (take-last 1 parts))]
    (partition-by sign (apply concat filled))))

(defn ts-peaks
  "Takes a numeric time series and returns a sequence corresponding to
   local peaks. Each item is a map like
   `{:start i, :end i+d, :duration d, :value v}`."
  [xs]
  (let [diffs (cons 0 (map - (next xs) xs))
        i-diff-xs (map vector (range (count xs)) diffs xs)
        slopes (partition-by (comp sign second) i-diff-xs)
        peaks (map (fn [[p c n]]
                     (let [[_ pd _] (first p)
                           [i _ cv] (first c)
                           [ni nd _] (first n)]
                       (when (and (pos? pd)
                                  (neg? nd)) ;; peak
                         {:start i
                          :end (dec ni)
                          :duration (- ni i)
                          :value cv})))
                   (partition 3 1 slopes))]
    (remove nil? peaks)))


(defn history-peaks-parasites-fn
  "Returns a parasites selection function that gives the top n
   individuals by fitness and up to m chosen from the history of
   generation champions. Champions are eligible if their fitness is on
   a local peak (of the time series), i.e. there are lower fitnesses
   both before and after it, and no higher fitnesses within that
   window.

   Generation champions are looked up in the history vector using keys
   `[popid :best]`."
  [n m]
  (fn [xs h popid]
    (let [champs (mapv #(get-in % [popid :best]) h)
          chfit (map evo/get-fitness champs)
          fitpeaks (sort-by :value (ts-peaks chfit))
          selpeaks (->> fitpeaks
                        (take (* 2 m))
                        (shuffle)
                        (take m))
          hist-champs (map (fn [peak]
                             (nth champs (:end peak)))
                           selpeaks)
          cur-n (take-last n (sort-by evo/get-fitness xs))]
      (concat cur-n hist-champs))))

(defn dominance-tournament
  "Conducts a dominance tournament (Stanley & Miikkulainen, 2002) to
   determine progress in a coevolution run. Give the full sequence of
   champions from the :a and :b populations. The superiority function
   takes any two individuals and evaluates them against each other,
   returning true if the first individual is strictly superior to the
   second.
   Returns the successive dominant strategies as a sequence of
   [generation-number individual popn-id]."
  [a-champions b-champions superiority-fn]
  ;; TODO
  )
