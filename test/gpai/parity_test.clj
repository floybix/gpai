(ns gpai.parity-test
  (:use clojure.test)
  (:require [gpai.problems.parity :as parity]
            [gpai.lang.logic :as logic]
            [gpai.utils :refer [arity]]
            [gpai.cgp :as cgp]
            [gpai.evolution :as evo]))

(defn n-parity-fitness
  [n]
  (let [fs `#{logic/_and_
              logic/_or_
              logic/_nand_
              logic/_nor_}
        lang (map (juxt identity arity) fs)
        inm (mapv #(str "i" %) (range n))
        opts {:erc-prob 0.0}
        inputs (parity/gen-inputs n)
        fitness (fn [gm]
                  (let [f (comp boolean first (cgp/genome->fn gm))]
                    (parity/fitness-fn inputs f)))
        regen (evo/fullymixed-regeneration-fn
               cgp/mutate
               #(assert false)
               :select-n 1
               :mutation-prob 1.0)
        init-popn (repeatedly 5 #(cgp/rand-genome inm 100 1 lang opts))
        soln (time (evo/evolve init-popn
                               fitness
                               regen
                               {:target 1.0
                                :n-gens 3000
                                :progress-every 1000
                                :snapshot-secs nil}))]
    (:fit-max (last (:history soln)))))

(deftest even-3-parity-test
  (testing "Can evolve a solution using cgp. Even 3-parity."
    (is (== (n-parity-fitness 3) 1.0) "Found a solution")))

(deftest even-4-parity-test
  (testing "Can evolve a solution using cgp. Even 4-parity."
    (is (>= (n-parity-fitness 4) 0.75) "Reasonable solution")))

(deftest even-5-parity-test
  (testing "Can evolve a solution using cgp. Even 5-parity."
    (is (>= (n-parity-fitness 5) 0.75) "Reasonable solution")))
