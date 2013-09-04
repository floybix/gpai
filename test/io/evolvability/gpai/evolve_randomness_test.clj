(ns io.evolvability.gpai.evolve-randomness-test
  (:use clojure.test
        [clojure.pprint :only [pprint]])
  (:require (io.evolvability.gpai [lang-integer :as langi]
                                  [cgp :as cgp]
                                  [cgp-viz :as cgp-viz]
                                  [evolution :as evo]
                                  [coevolution :as coevo]
                                  [utils :as utils])
            [io.evolvability.gpai.problems.randomness :as rness]
            [clojure.data.generators :as gen]))

(defn print-fitness-ranges
  "Plot fitness with a text line mapped to fitness range [-16 16]."
  [i xs h]
  (let [strata (group-by coevo/get-popn xs)
        width 80
        fit-min -16
        fit-max 16
        fit-range (- fit-max fit-min)
        line-scale (fn [fit] (* width (/ (- fit fit-min) fit-range)))
        i-0 (line-scale 0)
        prefix (format "%4d |" i)
        postfix "|=best"]
    (doseq [[id sxs] (sort strata)]
      (let [me (if (= :a id) "g" "D")
            fso (sort (map evo/get-fitness-0 sxs))
            fit-lo (first fso)
            fit-hi (last fso)
            i-lo (line-scale fit-lo)
            i-hi (line-scale fit-hi)
            my-line (take width (concat (repeat i-lo \space)
                                        (repeat (- i-hi i-lo) me)
                                        (repeat \space)))
            my-line (assoc (vec my-line) i-0 "0")]
        (println (str prefix
                      (apply str my-line)
                      postfix))))))

(deftest evolve-randomness-test
  (testing "Can coevolve using cgp."
    (let [lang (into langi/lang [[`rness/_nth-bit_ 2]
                                 [0]])
          gen-inm ["seed"]
          gen-nout 2
          disc-inm ["x" "i1" "i2" "i3" "i4" "i5" "i6"]
          disc-n (count disc-inm)
          mag 1024
          opts {:data-type 'long
                :erc-prob 0.25
                :erc-gen #(gen/long 0 16)}
          fitness (fn [gen disc]
                    (let [gen-f (cgp/function gen)
                          disc-f (cgp/function disc)]
                      (rness/duel 16 16 mag gen-f disc-f disc-n)))
          regen (evo/regenerate-fn cgp/mutate
                                   nil ;; no crossover
                                   :select-n 1
                                   :elitism 1
                                   :mutation-prob 1.0)
          init-gens (repeatedly 30 #(cgp/rand-genome gen-inm 100 gen-nout lang opts))
          init-discs (repeatedly 30 #(cgp/rand-genome disc-inm 100 disc-n lang opts))
          soln (time (coevo/coevolve init-gens init-discs
                                     fitness
                                     (coevo/history-peaks-parasites-fn 2 6)
                                     regen
                                     {:n-gens 100
                                      :progress! print-fitness-ranges
                                      :progress-every 10}))]
      (let [gchamps (mapv #(get-in % [:a :best]) (:history soln))
            dchamps (mapv #(get-in % [:b :best]) (:history soln))
            gpeaks (utils/ts-peaks (map evo/get-fitness gchamps))
            dpeaks (utils/ts-peaks (map evo/get-fitness dchamps))
            gsel (map (fn [p] (nth gchamps (:end p))) gpeaks)
            dsel (map (fn [p] (nth dchamps (:end p))) dpeaks)]
        (println)
        (println "generator fitness sequence:")
        (println (map-indexed vector (map (comp long evo/get-fitness) gchamps)))
        (println)
        (println "discriminator fitness sequence:")
        (println (map-indexed vector (map (comp long evo/get-fitness) dchamps)))
        (println)
        (println "generator fitness peaks:")
        (dorun (map println gpeaks))
        (println)
        (println "discriminator fitness peaks:")
        (dorun (map println dpeaks))
        (println)
        (println "generator seqs in each peak (seed 3):")
        (doseq [gen gsel
                :let [gen-f (cgp/function gen)]]
          (println (rness/gen-seq gen-f 16 mag 3)))
        (println))
      ;; print out final results
      (let [final (last (:history soln))
            gen (:best (:a final))
            disc (:best (:b final))
            gen-f (cgp/function gen)
            disc-f (cgp/function disc)
            run-gen (partial rness/gen-seq gen-f 16 mag)
            run-disc (partial rness/nonrandomness-score disc-f disc-n)]
        (println "FINAL RESULTS")
        (println)
        (println "generator sequence:")
        (println "seed 1")
        (println (run-gen 1))
        (println "seed 2")
        (println (run-gen 2))
        (println "seed 3")
        (println (run-gen 3))
        (println "seed 12")
        (println (run-gen 12))
        (println)
        (println "discriminator scores on generator seqs above:")
        (println (run-disc (run-gen 1)))
        (println (run-disc (run-gen 2)))
        (println (run-disc (run-gen 3)))
        (println (run-disc (run-gen 12)))
        (println "discriminator scores on random seqs:")
        (let [r1 (rness/rand-seq 16 mag 1)
              r2 (rness/rand-seq 16 mag 2)
              r3 (rness/rand-seq 16 mag 3)
              r12 (rness/rand-seq 16 mag 12)]
          (println (run-disc r1))
          (println (run-disc r2))
          (println (run-disc r3))
          (println (run-disc r12))
          (println "the random seqs:")
          (println r1)
          (println r2)
          (println r3))
        (println "genome of generator.")
        (cgp-viz/viz-active-nodes gen :name "generator" :open? false)
        (println "...")
        (println "genome of discriminator.")
        (cgp-viz/viz-active-nodes disc :name "discriminator" :open? false)
        ))))
