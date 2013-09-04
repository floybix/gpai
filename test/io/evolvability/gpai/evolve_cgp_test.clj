(ns io.evolvability.gpai.evolve-cgp-test
  (:use clojure.test)
  (:require (io.evolvability.gpai [lang-float :as langf]
                                  [cgp :as cgp]
                                  [evolution :as evo])
            [io.evolvability.gpai.problems.circle :as circle]
            [clojure.data.generators :as gen]
            [clojure.pprint :as pp]))

(deftest evolution-test
  (testing "Can evolve a solution using cgp. Classify points as in
   or out of a circle of given radius."
    (let [lang (conj langf/lang [0.0])
          inm (mapv str circle/inputs)
          opts {:data-type 'double
                :erc-prob 0.25
                :erc-gen #(* (gen/double) 5.0)}
          inputs (circle/grid-inputs 4 (range 1 5))
          fitness (fn [gm]
                    ;; take a positive number as classified "true"
                    (let [f (comp pos? first (cgp/function gm))]
                      (circle/fitness-fn inputs f)))
          regen (evo/regenerate-fn cgp/mutate
                                   nil ;; no crossover
                                   :select-n 1
                                   :mutation-prob 1.0)
          init-popn (repeatedly 5 #(cgp/rand-genome inm 50 1 lang opts))
          soln (time (evo/simple-evolve init-popn
                                        fitness
                                        regen
                                        {:target 1.0
                                         :n-gens 3000
                                         :progress-every 500}))]
      (is (= 5 (count (:popn soln))) "Final population count")
      (is (vector? (:nodes (:best (last (:history soln))))) "Final solution accessible")
      (is (every? number? (map :fit-max (:history soln))) "Fitnesses are numbers")
      (is (> (:fit-max (last (:history soln))) 0.8) "Reasonable solution")
      ;; print out grid of hits/misses
      (let [gm (:best (last (:history soln)))
            f (comp pos? first (cgp/function gm))]
        (circle/print-solution 4 [1 2.5 3.5] f)
        (println "Genome expression:")
        (binding [pp/*print-suppress-namespaces* true]
          (println inm)
          (pp/pprint (cgp/genome->expr gm)))))))
