(ns io.evolvability.gpai.evolve-tree-test
  (:use clojure.test)
  (:require (io.evolvability.gpai [lang-float :as langf]
                                  [tree :as tree]
                                  [evolution :as evo])
            [io.evolvability.gpai.problems.circle :as circle]
            [clojure.data.generators :as gen]
            [clojure.pprint :as pp]))

(deftest evolution-test
  (testing "Can evolve a solution using tree gp. Classify points as in
   or out of a circle of given radius."
    (let [lang (conj langf/lang [0.0])
          ins circle/inputs
          opts {:erc-prob 0.25
                :erc-gen #(* (gen/double) 5.0)}
          inputs (circle/grid-inputs 4 (range 1 5))
          fitness (fn [gm]
                    ;; take a positive number as classified "true"
                    (let [f (comp pos? (tree/function gm))]
                      (circle/fitness-fn inputs f)))
          regen (evo/negative-selection-fn 2 tree/mutate-subtree
                                           tree/crossover-subtrees
                                           :elitism 1)
          init-popn (repeatedly 10 #(tree/rand-genome ins lang opts))
          soln (time (evo/simple-evolve init-popn
                                        fitness
                                        regen
                                        {:target 1.0
                                         :n-gens 500
                                         :progress! (juxt evo/print-progress
                                                          tree/print-codesizes)
                                         :progress-every 50}))]
      (is (= 500 (count (:history soln))) "Generation count")
      (is (= 10 (count (:popn soln))) "Final population count")
      (is (sequential? (:expr (:best (last (:history soln)))))
          "Final solution accessible")
      (is (every? number? (map :fit-max (:history soln))) "Fitnesses are numbers")
      (is (> (:fit-max (last (:history soln))) 0.8) "Reasonable solution")
      ;; print out grid of hits/misses
      (let [gm (:best (last (:history soln)))
            f (comp pos? (tree/function gm))]
        (circle/print-solution 4 [1 2.5 3.5] f)
        (println "Genome expression:")
        (binding [pp/*print-suppress-namespaces* true]
          (pp/pprint (tree/genome->expr gm)))))))
