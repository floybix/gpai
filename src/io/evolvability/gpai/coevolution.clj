(ns io.evolvability.gpai.coevolution
  (:require [io.evolvability.gpai.evolution :as evo]))

(defn stratified-basic-distil
  [xs]
  (let [strata (group-by (comp ::popn meta) xs)]
    (zipmap (keys strata)
            (map evo/basic-distil (vals strata)))))

(defn stratified-print-progress
  [xs i]
  (let [strata (group-by (comp ::popn meta) xs)]
    (doseq [[id sxs] (sort strata)]
      (print (str id " "))
      (evo/print-progress sxs i))))

(defn coevolve
  "High-level coevolution function, where fitness is determined by
   one-on-one competitions. As 'host-parasite coevolution', it
   maintains two separate populations (:a and :b); individuals
   normally only duel opponents from the other population.
   * init-popn-a is the initial :a population collection.
   * init-popn-b is the initial :b population collection.
   * fitness function is applied to any two individuals, and returns
     their respective fitness scores (a vector of 2 numbers),
     typically the number of fitness cases won by each. The population
     identifier (:a or :b) is on each individual's meta ::popn.
   * parasites function is used to select a few individuals as worthy
     opponents (based on quality and diversity). It is called with 2
     arguments:
     * the sub-population from which to select representatives. This
       is the previous, fitness-evaluated population, or the initial
       population in the first generation.
     * the history vector.
   * regenerate functions are applied to each fitness-evaluated
     sub-population. They should return a corresponding new population
     with new individuals that are typically derived by mutation or
     crossover.
   Other options are passed on to `evolve-discrete`."
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
    {:as options
     :keys [distil progress!]
     :or {distil #'stratified-basic-distil
          progress! #'stratified-print-progress}}]
     (let [eval-fitness (fn [host parasites]
                          (let [fs (map #(fitness host %) parasites)
                                fit (reduce + (map first fs))]
                            (vary-meta host assoc :evo/fitness fit)))
           brand (fn [id x] (vary-meta x assoc ::popn id))
           brand-all (fn [id xs] (map (partial brand id) xs))
           init-popn (concat (brand-all :a init-popn-a)
                             (brand-all :b init-popn-b))
           eval-popn-fitness (fn [xs prev-xs history]
                               (let [strata (group-by (comp ::popn meta) xs)
                                     pstrata (group-by (comp ::popn meta) prev-xs)
                                     a-paras (parasites-fn-a (or (:a pstrata) (:a strata))
                                                             history)
                                     b-paras (parasites-fn-b (or (:b pstrata) (:b strata))
                                                             history)]
                                 (concat (map #(eval-fitness % b-paras) (:a strata))
                                         (map #(eval-fitness % a-paras) (:b strata)))))
           regenerate (fn [xs]
                        (let [strata (group-by (comp ::popn meta) xs)]
                          (concat (brand-all :a (regenerate-a (:a strata)))
                                  (brand-all :b (regenerate-b (:b strata))))))]
       (evo/evolve-discrete init-popn eval-popn-fitness regenerate options))))

(defn top-n-parasites-fn
  "Returns a basic parasites selection function that only considers
   fitness, not diversity, and not history."
  [n]
  (fn [xs _]
   (let [sortd (sort-by (comp :evo/fitness meta) xs)]
     (take-last n sortd))))

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
