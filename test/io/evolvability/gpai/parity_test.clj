(ns io.evolvability.gpai.parity-test
  (:use clojure.test)
  (:require (io.evolvability.gpai [lang-logic :as logic]
                                  [cgp :as cgp]
                                  [evolution :as evo])
            [io.evolvability.gpai.problems.parity :as parity]))

(defn n-parity-fitness
  [n]
  (let [lang `[[logic/_and_ 2]
               [logic/_or_ 2]
               [logic/_nand_ 2]
               [logic/_nor_ 2]]
        inm (mapv #(str "i" %) (range n))
        opts {:erc-prob 0.0}
        inputs (parity/gen-inputs n)
        fitness (fn [gm]
                  (let [f (comp boolean first (cgp/function gm))]
                    (parity/fitness-fn inputs f)))
        regen (evo/regenerate-fn cgp/mutate
                                 nil ;; no crossover
                                 :select-n 1
                                 :mutation-prob 1.0)
        init-popn (repeatedly 5 #(cgp/rand-genome inm 100 1 lang opts))
        soln (time (evo/simple-evolve init-popn
                                      fitness
                                      regen
                                      {:target 1.0
                                       :n-gens 3000
                                       :progress-every 1000}))]
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
