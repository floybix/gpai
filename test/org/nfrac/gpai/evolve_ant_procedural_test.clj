(ns org.nfrac.gpai.evolve-ant-procedural-test
  (:use clojure.test)
  (:require (org.nfrac.gpai [tree :as tree]
                            [cgp-viz :as cgp-viz]
                            [evolution :as evo]
                            [utils :as utils])
            (org.nfrac.gpai.problems.ant-trail [core :as ant]
                                               [procedural :as ant-p])
            [clojure.data.generators :as gen]
            [clojure.pprint :as pp]))

(deftest evolve-ant-procedural-test
  (testing "Can evolve procedural ant program using tree GP."
    (let [lang ant-p/lang
          opts {:max-expr-depth 5}
          food ant/santafe-food
          loc ant/start-loc
          dir ant/start-dir
          ;; run-ant uses thread-local vars so use 'pmap' below
          fitness (fn [gm]
                    (let [f! (tree/function gm)]
                      (-> (ant-p/run-ant f! food loc dir)
                          (ant-p/fitness))))
          regen (evo/negative-selection-fn 1 tree/mutate-subtree
                                           tree/crossover-subtrees
                                           :select-n 5
                                           :elitism 1)
          init-popn (repeatedly 50 #(tree/rand-genome [] lang opts))
          soln (time (evo/simple-evolve init-popn
                                        fitness
                                        regen
                                        {:map-fn #'pmap
                                         :n-gens 500
                                         :progress-every 50}))]
      (let [gm (:best (last (:history soln)))
            f! (tree/function gm)
            state (-> (ant-p/run-ant f! food loc dir)
                      future deref)]
        (ant/print-ant-trail (:path state) food)
        (println "Genome expression:")
        (binding [pp/*print-suppress-namespaces* true]
          (pp/pprint (tree/genome->expr gm)))))))
