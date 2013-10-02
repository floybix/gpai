(ns io.evolvability.gpai.evolve-ant-functional-test
  (:use clojure.test)
  (:require (io.evolvability.gpai [icgp :as icgp]
                                  [cgp-viz :as cgp-viz]
                                  [lang-typed-basic :as langt]
                                  [evolution :as evo]
                                  [utils :as utils])
            (io.evolvability.gpai.problems.ant-trail [core :as ant]
                                                     [functional :as ant-f])
            [clojure.data.generators :as gen]
            [clojure.pprint :as pp]))

(defn handcoded-functional-ant
  [food-ahead?
   state-a
   state-b]
  (let [act-0 (if food-ahead? ::ant-f/move ::ant-f/turn-right)
        nofood-count (if food-ahead? 0 (inc state-a))
        turning? (not (< nofood-count (+ 1 4)))
        act (if turning? act-0 ::ant-f/move)
        sa (if turning? nofood-count 0)]
    [act sa state-b]))

(defn evolve-functional-ant
  [n-gens]
  (let [lang (vec (concat ant-f/lang-actions
                          langt/lang-boolean
                          langt/lang-long))
        opts {}
        food ant/santafe-food
        loc ant/start-loc
        dir ant/start-dir
        fitness (fn [gm]
                  (let [f (icgp/function gm)]
                    (-> (ant-f/run-ant f food loc dir)
                        (ant-f/fitness))))
        regen (comp (evo/negative-selection-fn 1 icgp/mutate-out-ids nil
                                               :elitism 1)
                    #(map icgp/vary-neutral % (repeat 200))
                    #(map icgp/tick %))
        init-popn (repeatedly 5 #(icgp/rand-genome ant-f/inputs
                                                   ant-f/constants
                                                   ant-f/outputs
                                                   lang 100 opts))]
    (evo/simple-evolve init-popn
                       fitness
                       regen
                       {:map-fn #'pmap
                        :n-gens n-gens
                        :progress! (fn [i xs _]
                                     (let [sortd (sort-by evo/get-fitness-0 xs)
                                           gm (last sortd)]
                                       (println i)
                                       (println "Genome expression:")
                                       (binding [pp/*print-suppress-namespaces* true]
                                         (pp/pprint (icgp/out-exprs gm)))
                                       (println i "fitness" (double (evo/get-fitness gm)))
                                       (flush)))
                        :progress-every 100})))

(deftest evolve-functional-ant-test
  (testing "Can evolve functional ant program using ICGP."
    (let [food ant/santafe-food
          soln (time (evolve-functional-ant 1000))
          gm (:best (last (:history soln)))
          f (icgp/function gm)
          state (ant-f/run-ant f food ant/start-loc ant/start-dir)]
      (ant/print-ant-trail (:path state) food)
      (println "Genome expression:")
      (binding [pp/*print-suppress-namespaces* true]
        (pp/pprint (map-indexed vector (icgp/out-exprs gm)))))))

(defn print-santafe-path
  [gm]
  (let [f (icgp/function gm)
        state (ant-f/run-ant f ant/santafe-food ant/start-loc ant/start-dir)]
    (ant/print-ant-trail (:path state) ant/santafe-food)
    state))

(comment
  (require :reload-all 'io.evolvability.gpai.evolve-ant-test)
  (in-ns 'io.evolvability.gpai.evolve-ant-test)
  (def state (ant-f/run-ant handcoded-functional-ant ant/santafe-food ant/start-loc ant/start-dir))
  (ant/print-ant-trail (:path state) ant/santafe-food)
  (def soln (evolve-functional-ant 1000))
  (def gm (:best (last (:history soln))))
  (print-santafe-path gm)
  (println "expression:")
  (binding [pp/*print-suppress-namespaces* true]
    (pp/pprint (map-indexed vector (icgp/out-exprs gm))))
  )
