(ns io.evolvability.gpai.evolve-randomness-test
  (:use clojure.test)
  (:require (io.evolvability.gpai [lang-integer :as langi]
                                  [utils :refer [arity]]
                                  [cgp :as cgp]
                                  [evolution :as evo]
                                  [coevolution :as coevo])
            [io.evolvability.gpai.problems.randomness :as rness]))

(deftest evolve-randomness-test
  (testing "Can coevolve using cgp."
    (let [fs (conj langi/funcset `rness/_nth-bit_ 0)
          lang (map (juxt identity arity) fs)
          gen-inm ["seed"]
          gen-nout 2
          disc-inm (mapv str rness/discriminator-inputs)
          disc-nout (count disc-inm)
          opts {:erc-prob 0.25
                :erc-gen #(rand-int 16)}
          fitness (fn [gen disc]
                    (let [gen-f (cgp/genome->fn gen)
                          disc-f (cgp/genome->fn disc)]
                      (rness/duel 16 16 gen-f disc-f)))
          regen (evo/regenerate-fn cgp/mutate
                                   nil ;; no crossover
                                   :select-n 3
                                   :mutation-prob 1.0)
          init-gens (repeatedly 10 #(cgp/rand-genome gen-inm 100 gen-nout lang opts))
          init-discs (repeatedly 10 #(cgp/rand-genome disc-inm 100 disc-nout lang opts))
          soln (time (coevo/coevolve init-gens init-discs
                                     fitness
                                     (coevo/history-peaks-parasites-fn 2 4)
                                     regen
                                     {:n-gens 100
                                      :progress-every 5}))]
      ;; print out results
      (let [final (last (:history soln))
            gen (:best (:a final))
            disc (:best (:b final))
            gen-f (cgp/genome->fn gen)
            disc-f (cgp/genome->fn disc)]
        (println "final distil:")
        (binding [*print-meta* true]
          (prn final))
        (println)
        (println "generator sequence:")
        (println "seed 1")
        (println (rness/gen-seq gen-f 32 1))
        (println "seed 12")
        (println (rness/gen-seq gen-f 32 12))
        (println)
        (println "discriminator fitnesses:")
        (println (sort (map (juxt (comp :io.evolvability.gpai.coevolution/popn meta)
                                  evo/get-fitness)
                            (:popn soln))))
        (println "discriminator scores on generator seqs above:")
        (println (rness/nonrandomness-score disc-f (rness/gen-seq gen-f 32 1)))
        (println (rness/nonrandomness-score disc-f (rness/gen-seq gen-f 32 12)))
        (println "discriminator scores on random seqs:")
        (let [r1 (rness/rand-seq 32 1)
              r2 (rness/rand-seq 32 12)]
          (println (rness/nonrandomness-score disc-f r1))
          (println (rness/nonrandomness-score disc-f r2))
          (println "the random seqs:")
          (println r1)
          (println r2))
        (println "genome of generator.")
        (cgp/viz-active-nodes gen :name "generator")
        (println "...")
        (println "genome of discriminator.")
        (cgp/viz-active-nodes disc :name "discriminator")
        ))))
