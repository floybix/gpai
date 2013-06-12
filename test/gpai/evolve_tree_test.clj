(ns gpai.evolve-tree-test
  (:use clojure.test)
  (:require [gpai.lang.arith :as arith]
            [gpai.lang.core :as lang]
            [gpai.tree :as tree]
            [gpai.evolution :as evo]
            [clojure.pprint :as pp]))

(deftest evolution-test
  (testing "Can evolve a solution using tree gp. Classify points as in
  or out of a circle of given radius."
    (let [fs arith/funcset
          fm (map (juxt identity lang/arity) fs)
          ts '[r y x 0]
          test-inputs (for [r [1 2 3.5]
                            y (range -4 (inc 4))
                            x (range -4 (inc 4))]
                        [r y x])
          actual-f (fn [[r y x]] (<= (+ (* x x) (* y y))
                                    (* r r)))
          test-actual (map actual-f test-inputs)
          fitness (fn [expr]
                    (let [f (comp pos? (lang/fn-from-expr '[[r y x]] expr))
                          test-out (map f test-inputs)]
                      (/ (count (filter true? (map = test-out test-actual)))
                         (count test-inputs))))
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
                                 :progress-every 50
                                 :snapshot-secs nil))]
      (is (== 1000 (count (:history soln))) "Generation count")
      (is (== 4 (count (:pop soln))) "Final population count")
      (is (sequential? (:best (last (:history soln)))) "Final solution accessible")
      (is (every? number? (map :fit-max (:history soln))) "Fitnesses are numbers")
      (is (> (:fit-max (last (:history soln))) 0.8) "Good solution")
      ;; print out grid of hits/misses
      (let [expr (:best (last (:history soln)))
            f (comp pos? (lang/fn-from-expr '[[r y x]] expr))
            dat (map (fn [[r y x]] {:r r, :y y, :x x
                                   :actual (actual-f [r y x])
                                   :output (f [r y x])})
                     test-inputs)
            hitchar (fn [{:keys [output actual]}]
                      (condp = [output actual]
                        [true true] \O
                        [false true] \x
                        [true false] \?
                        [false false] \_))]
        (doseq [r-dat (partition-by :r dat)]
          (println)
          (println "r =" (:r (first r-dat)))
          (doseq [y-dat (partition-by :y r-dat)
                  :let [y (:y (first y-dat))]]
            (println (apply str (format "%+3d" y) ": "
                            (interpose " " (map hitchar y-dat))))))))))
