(ns gpai.evolution.coevolution
  (:require [gpai.evolution :as evo]))

(defn summarise-strata
  [xs]
  (let [strata (group-by (comp ::popn meta) xs)]
    (zipmap (keys strata)
            (map evo/summarise-keep-best (vals strata)))))

(defn print-progress-strata
  [xs i]
  (let [strata (group-by (comp ::popn meta) xs)]
    (doseq [[id sxs] (sort strata)]
      (print (str id " "))
      (evo/print-progress sxs i))))

(defn co-evolve
  "High-level co-evolution function, where fitness is determined by
   one-on-one competitions. As 'host/parasite evolution', maintains
   two separate populations (:a and :b); individuals normally only
   duel opponents from the other population.
   * init-popn-a is the initial :a population as a sequence.
   * init-popn-b is the initial :b population as a sequence.
   * fitness function is applied to any two individuals, and returns
     their respective fitness scores (a vector of 2 numbers),
     typically the number of fitness cases won by each. The population
     identifier (:a or :b) is in each individual's meta ::popn.
   * parasites function is used to select a few individuals as worthy
     opponents (based on quality and diversity). It is called with 2
     arguments:
     * the sub-population from which to select representatives. This
     is the previous, fitness-evaluated population -- where fitness
     values are in each individual's meta :fitness -- except in the
     first generation when no fitness values exist.
     * the history vector (from :summarise option).
   * regeneration functions are applied to each population sequence
     where fitness values for each individual are in meta :fitness.
     Return a corresponding new population with new individuals that
     are typically derived by mutation or crossover."
  ([init-popn-a init-popn-b
    fitness
    parasites-fn
    regenerate
    options]
     (co-evolve init-popn-a init-popn-b
                fitness
                parasites-fn parasites-fn
                regenerate regenerate
                options))
  ([init-popn-a init-popn-b
    fitness
    parasites-fn-a parasites-fn-b
    regenerate-a regenerate-b
    {:as options
     :keys [summarise progress]
     :or {summarise #'summarise-strata
          progress #'print-progress-strata}}]
     (let [eval-fitness (fn [host parasites]
                          (let [fs (map #(fitness host %) parasites)
                                fit (reduce + (map first fs))]
                            (vary-meta host assoc :fitness fit)))
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
       (evo/evolve-general init-popn eval-popn-fitness regenerate options))))

(defn top-n-parasites-fn
  "Returns a basic parasites selection function that only considers
   fitness, not diversity, and not history."
  [n]
  (fn [xs _]
   (let [sortd (sort-by (comp :fitness meta) xs)]
     (take-last n sortd))))

(defn dominance-tournament
  "Conducts a dominance tournament (Stanley & Miikkulainen, 2002) to
   determine progress in a co-evolution run. Give the full sequence of
   champions from the :a and :b populations. The superiority function
   takes any two individuals and evaluates them against each other,
   returning true if the first individual is strictly superior to the
   second.
   Returns the successive dominant strategies as a sequence of
   [generation-number individual popn-id]."
  [a-champions b-champions superiority-fn]
  ;; TODO
  )
