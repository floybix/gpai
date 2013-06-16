(ns gpai.evolve-tree-test
  (:use clojure.test)
  (:require [gpai.problems.circle :as circle]
            [gpai.lang.arith :as arith]
            [gpai.lang.core :as lang]
            [gpai.tree :as tree]
            [gpai.evolution :as evo]))

(deftest evolution-test
  (testing "Can evolve a solution using tree gp. Classify points as in
  or out of a circle of given radius."
    (let [fs arith/funcset
          fm (map (juxt identity lang/arity) fs)
          ts (into circle/inputs [0])
          fitness (fn [expr]
                    (let [f (comp pos? (lang/fn-from-expr [circle/inputs] expr))]
                      (circle/fitness-fn f)))
          regen (evo/fullymixed-regeneration-fn (partial tree/mutate-subtree fm ts)
                                                tree/crossover-subtrees
                                                :select-n 1
                                                :mutation-prob 0.9)
          init-pop (repeatedly 4 #(tree/gen-expr fm ts))
          soln (time (evo/evolve init-pop
                                 fitness
                                 regen
                                 evo/summarise-keep-best
                                 :n-gens 1000
                                 :progress (juxt evo/print-progress
                                                 tree/print-codesizes)
                                 :progress-every 100
                                 :snapshot-secs nil))]
      (is (== 1000 (count (:history soln))) "Generation count")
      (is (== 4 (count (:pop soln))) "Final population count")
      (is (sequential? (:best (last (:history soln)))) "Final solution accessible")
      (is (every? number? (map :fit-max (:history soln))) "Fitnesses are numbers")
      (is (> (:fit-max (last (:history soln))) 0.8) "Reasonable solution")
      ;; print out grid of hits/misses
      (let [expr (:best (last (:history soln)))
            f (comp pos? (lang/fn-from-expr '[[r y x]] expr))]
        (circle/print-solution f)))))
