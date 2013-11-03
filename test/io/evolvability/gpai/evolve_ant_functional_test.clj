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

(defn a-handcoded-functional-ant
  [food-ahead?
   state-a
   state-b]
  (let [act-0 (if food-ahead? ::ant-f/move ::ant-f/turn-right)
        turn-count (if food-ahead? 0 (inc state-a))
        turning? (< turn-count (+ 1 4))
        act (if turning? act-0 ::ant-f/move)
        sa (if turning? turn-count 0)]
    [act sa state-b]))

(deftest a-handcoded-functional-ant-test
  (testing "Hand-coded functional ant function runs."
   (let [state (ant-f/run-ant a-handcoded-functional-ant
                              ant/santafe-food
                              ant/start-loc ant/start-dir)]
     (ant/print-ant-trail (:path state) ant/santafe-food))))

(defn evolve-functional-ant
  [n-gens]
  (let [lang (vec (concat ant-f/lang-actions
                          langt/lang-boolean
                          langt/lang-long))
        food ant/santafe-food
        loc ant/start-loc
        dir ant/start-dir
        fitness (fn [gm]
                  (let [f (icgp/function gm)]
                    (-> (ant-f/run-ant f food loc dir)
                        (ant-f/fitness))))
        regen (evo/negative-selection-fn 1 icgp/mutate nil
                                         :elitism 1)
        init-popn (icgp/rand-genomes 5 ant-f/inputs
                                     ant-f/constants
                                     ant-f/outputs
                                     50 lang {})]
    (evo/simple-evolve init-popn
                       fitness
                       regen
                       {:map-fn #'pmap
                        :n-gens n-gens
                        :progress! (juxt evo/print-progress
                                         icgp/print-codesizes)
                        :progress-every 100})))

(deftest evolve-functional-ant-test
  (testing "Can evolve functional ant program using icgp."
    (let [food ant/santafe-food
          soln (time (evolve-functional-ant 2000))
          gm (:best (last (:history soln)))
          f (icgp/function gm)
          state (ant-f/run-ant f food ant/start-loc ant/start-dir)]
      (ant/print-ant-trail (:path state) food)
      (println "Genome viz:")
      (cgp-viz/viz-active-nodes gm :name "ant-functional" :open? false)
      (binding [pp/*print-suppress-namespaces* true]
        (println "Genome function:")
        (pp/pprint (icgp/genome->expr gm))
        (println "Genome expression:")
        (pp/pprint (map-indexed vector (icgp/out-exprs gm)))))))
