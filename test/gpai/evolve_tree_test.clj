(ns gpai.evolve-tree-test
  (:use clojure.test)
  (:require [gpai.problems.circle :as circle]
            [gpai.lang.arith :as arith]
            [gpai.utils :refer [arity]]
            [gpai.tree :as tree]
            [gpai.evolution :as evo]))

(deftest evolution-test
  (testing "Can evolve a solution using tree gp. Classify points as in
   or out of a circle of given radius."
    (let [fs (conj arith/funcset-real 0)
          lang (map (juxt identity arity) fs)
          ins circle/inputs
          opts {:erc-prob 0.25
                :erc-gen #(rand 5.0)}
          train-inputs (circle/gen-inputs [1 2.5 3.5] 4)
          fitness (fn [gm]
                    ;; take a positive number as classified "true"
                    (let [f (comp pos? (tree/genome->fn gm))]
                      (circle/fitness-fn train-inputs f)))
          regen (evo/regenerate-fn tree/mutate-subtree
                                   tree/crossover-subtrees
                                   :select-n 1
                                   :mutation-prob 0.9)
          init-popn (repeatedly 5 #(tree/rand-genome ins lang opts))
          soln (time (evo/simple-evolve init-popn
                                        fitness
                                        regen
                                        {:n-gens 1000
                                         :progress! (juxt evo/print-progress
                                                          tree/print-codesizes)
                                         :progress-every 100}))]
      (is (== 1000 (count (:history soln))) "Generation count")
      (is (== 5 (count (:popn soln))) "Final population count")
      (is (sequential? (:expr (:best (last (:history soln)))))
          "Final solution accessible")
      (is (every? number? (map :fit-max (:history soln))) "Fitnesses are numbers")
      (is (> (:fit-max (last (:history soln))) 0.8) "Reasonable solution")
      ;; print out grid of hits/misses
      (let [gm (:best (last (:history soln)))
            f (comp pos? (tree/genome->fn gm))]
        (circle/print-solution train-inputs f)))))
