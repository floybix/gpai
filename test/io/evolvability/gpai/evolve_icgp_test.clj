(ns io.evolvability.gpai.evolve-icgp-test
  (:use clojure.test)
  (:require (io.evolvability.gpai [lang-typed-basic :as lang]
                                  [icgp :as icgp]
                                  [evolution :as evo])
            [io.evolvability.gpai.problems.circle :as circle]
            [clojure.data.generators :as gen]
            [clojure.pprint :as pp]))

(defn handcoded-circle
  [r y x]
  (< (lang/abs (- x y))
     (inc r)))

(comment
  (circle/print-solution 4 [2 3 4] handcoded-circle)
  )

(defn print-genome-atrophication
  [gm]
  (println "timestep:" (::icgp/timestep (meta gm)))
  (println "number of nodes:" (count (keep identity (:nodes gm))))
  (println "nodes [i last-use]:")
  (println (keep-indexed (fn [i x] (when x [i
                                           (::icgp/last-use (meta x))]))
                         (:nodes gm)))
  (println "active:")
  (println (sort (icgp/active-idx gm))))

(defn evolve-circle
  [n-gens]
  (let [lang lang/lang-double
        inputs [["r" Double] ["y" Double] ["x" Double]]
        constants [[0.0 Double]]
        outputs [Double]
        opts {:atrophy-steps 100
              :erc-prob 0.25
              :erc-gen #(vector (* (gen/double) 5.0) Double)}
        incases (circle/grid-inputs 4 [2 3 4])
        fitness (fn [gm]
                  (let [f (comp pos? first (icgp/function gm))]
                    (circle/fitness-fn incases f)))
        regen (comp (evo/negative-selection-fn 1 icgp/mutate-out-idx nil
                                               :elitism 1)
                    #(map icgp/add-rand-node %)
                    #(map icgp/atrophy-lru %)
                    #(map icgp/tick %))
        init-popn (repeatedly 5 #(icgp/rand-genome inputs constants outputs
                                                   lang 200 opts))]
    (evo/simple-evolve init-popn
                       fitness
                       regen
                       {:target 1.0
                        :n-gens n-gens
                        :progress! (fn [i xs _]
                                     (let [sortd (sort-by evo/get-fitness-0 xs)
                                           gm (last sortd)]
                                       (println i)
                                       (print-genome-atrophication gm)
                                       (println "Genome expression:")
                                       (binding [pp/*print-suppress-namespaces* true]
                                         (pp/pprint (first (icgp/out-exprs gm))))
                                       (println i "fitness" (double (evo/get-fitness gm)))
                                       (flush)))
                        :progress-every 500})))

(deftest evolution-test
  (testing "Can evolve a solution using icgp. Classify points as in
   or out of a circle of given radius."
    (let [soln (time (evolve-circle 5000))]
      (is (= 5 (count (:popn soln))) "Final population count")
      (is (vector? (:nodes (:best (last (:history soln))))) "Final solution accessible")
      (is (every? number? (map :fit-max (:history soln))) "Fitnesses are numbers")
      (is (> (:fit-max (last (:history soln))) 0.8) "Reasonable solution")
      ;; print out grid of hits/misses
      (let [gm (:best (last (:history soln)))
            f (comp pos? first (icgp/function gm))]
        (circle/print-solution 4 [2 3 4] f)
        (println "Genome expression:")
        (binding [pp/*print-suppress-namespaces* true]
          (pp/pprint (first (icgp/out-exprs gm))))))))
