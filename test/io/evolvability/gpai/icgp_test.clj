(ns io.evolvability.gpai.icgp-test
  (:use clojure.test)
  (:require (io.evolvability.gpai [icgp :refer :all]
                                  [cgp-viz :as cgp-viz]
                                  [lang-typed-basic :as langt])))

(deftest icgp-test
  (let [lang langt/lang-long
        consts [[0 Long]]
        ins [["a" Long]
             ["b" Long]
             ["c" Long]]
        outs [Long Long]]
    (testing "Basic operations on random ICGP genomes."
      (let [gm (rand-genome ins consts outs lang 16 {})]
        (is (every? associative? (:nodes gm)) "Nodes are maps")
        (is (= 20 (count (:nodes gm)))
            "Size of genome as given (includes inputs and constants)")
        (is (every? number? (active-ids gm)) "Active list is numbers")
        (is (re-seq #" -> " (with-out-str (cgp-viz/print-active-nodes gm)))
            "Print active nodes in directed dot format")
        (is (= 20 (count (:nodes (-> (mutate-out-ids gm)
                                     tick))))
            "Nodes remain after out-mutation & tick")
        (is (= 22 (count (:nodes (-> gm add-rand-node add-rand-node))))
            "Adding random nodes increases length")))
    (testing "Genome compiler"
      (let [gm (rand-genome ins consts outs lang 16 {})]
        (is (seq (genome->expr gm)))
        (is (not (nil? (function gm))))))))
