(ns gpai.evolve-cgp-test
  (:use clojure.test)
  (:require [gpai.problems.circle :as circle]
            [gpai.lang.arith :as arith]
            [gpai.lang.core :as lang]
            [gpai.cgp :as cgp]
            [gpai.evolution :as evo]))

(deftest evolution-test
  (testing "Can evolve a solution using cgp. Classify points as in
  or out of a circle of given radius."
    (let [fs arith/funcset-real
          fm (map (juxt identity lang/arity) fs)]
      (binding [cgp/*funcmap* fm]
        (let [inn (mapv str circle/inputs)
              train-inputs (circle/gen-inputs [1 2.5 3.5] 4)
              fitness (fn [gm]
                        (let [f (comp pos? first (cgp/genome->fn gm))]
                          (circle/fitness-fn train-inputs f)))
              regen (evo/fullymixed-regeneration-fn cgp/mutate
                                                    cgp/mutate
                                                    :select-n 1
                                                    :mutation-prob 1.0)
              init-popn (repeatedly 5 #(cgp/rand-genome inn 100 1))
              soln (time (evo/evolve init-popn
                                     fitness
                                     regen
                                     {:n-gens 1000
                                      :progress-every 100
                                      :snapshot-secs nil}))]
          (is (== 5 (count (:popn soln))) "Final population count")
          (is (vector? (:nodes (:best (last (:history soln))))) "Final solution accessible")
          (is (every? number? (map :fit-max (:history soln))) "Fitnesses are numbers")
          (is (> (:fit-max (last (:history soln))) 0.8) "Reasonable solution")
          ;; print out grid of hits/misses
          (let [gm (:best (last (:history soln)))
                f (comp pos? first (cgp/genome->fn gm))]
            (circle/print-solution train-inputs f)))))))
