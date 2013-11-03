(ns io.evolvability.gpai.evolve-icgp-test
  (:use clojure.test)
  (:require (io.evolvability.gpai [lang-typed-basic :as lang]
                                  [icgp :as icgp]
                                  [evolution :as evo])
            [io.evolvability.gpai.problems.circle :as circle]
            [clojure.data.generators :as gen]
            [clojure.pprint :as pp]))

(defn a-handcoded-circle
  [r y x]
  (< (lang/abs (- x y))
     (inc r)))

(deftest a-handcoded-circle-test
  (testing "Hand-coded circle classifier function runs."
   (circle/print-solution 4 [2 3 4] a-handcoded-circle)))

(defn evolve-circle
  [n-gens]
  (let [lang lang/lang-double
        inputs [["r" Double] ["y" Double] ["x" Double]]
        constants [[0.0 Double]]
        outputs [Double]
        opts {:erc-prob 0.25
              :erc-gen #(vector (* (gen/double) 5.0) Double)}
        incases (circle/grid-inputs 4 [1 2 3 4])
        fitness (fn [gm]
                  (let [f (comp pos? first (icgp/function gm))]
                    (circle/fitness-fn incases f)))
        regen (evo/negative-selection-fn 1 icgp/mutate
                                         nil ;; no crossover
                                         :elitism 1)
        init-popn (icgp/rand-genomes 5 inputs constants outputs
                                     50 lang opts)]
    (evo/simple-evolve init-popn
                       fitness
                       regen
                       {:target 1.0
                        :n-gens n-gens
                        :progress! (juxt evo/print-progress
                                         icgp/print-codesizes)
                        :progress-every 200})))

(deftest evolution-test
  (testing "Can evolve a solution using icgp. Classify points as in
   or out of a circle of given radius."
    (let [soln (time (evolve-circle 1000))]
      (is (= 5 (count (:popn soln))) "Final population count")
      (is (seq (:nodes (:best (last (:history soln))))) "Final solution accessible")
      (is (every? number? (map :fit-max (:history soln))) "Fitnesses are numbers")
      (is (> (:fit-max (last (:history soln))) 0.8) "Reasonable solution")
      ;; print out grid of hits/misses
      (let [gm (:best (last (:history soln)))
            f (comp pos? first (icgp/function gm))]
        (is (= (count (:nodes (:best (first (:history soln)))))
               (count (:nodes gm)))
            "Number of nodes unchanged.")
        (circle/print-solution 4 [1 2 3 4] f)
        (println "Genome expression:")
        (binding [pp/*print-suppress-namespaces* true]
          (pp/pprint (icgp/genome->expr gm))
          (pp/pprint (first (icgp/out-exprs gm))))))))
