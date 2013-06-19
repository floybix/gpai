(ns gpai.cgp-test
  (:use clojure.test
        gpai.cgp)
  (:require [gpai.lang.arith :as arith]
            [gpai.lang.core :as lang]))

(deftest graph-test
  (let [fs arith/funcset
        fm (map (juxt identity lang/arity) fs)
        inn ["a" "b" "c"]]
    (binding [*funcmap* fm]
      (testing "Can generate random graphs."
        (is (vector? (:in (rand-gene 5))) "Rand gene has inputs")
        (let [gm (rand-genome inn 16 2)]
          (is (vector? (:genes gm)) "Rand genome has genes")
          (is (= 16 (count (:genes gm))) "Size of genome as given")))
      (testing "Calculation of active genes"
        (let [gm (rand-genome inn 16 2)]
          (is (every? number? (active-idx gm)) "Active list contains numbers")
          (is (re-seq #" -> " (with-out-str (print-active-genes gm)))
              "Print active genes in directed dot format")
          ))
      (testing "Modify genomes"
        (let [gm (rand-genome inn 16 2)]
          (is (vector? (:genes (mutate gm))) "Mutate"))))))

(deftest eval-test
  (let [fs arith/funcset
        fm (map (juxt identity lang/arity) fs)
        inn ["a" "b" "c"]]
    (binding [*funcmap* fm]
      (testing "Eval generated expressions as a function"
        (let [gm (rand-genome inn 16 2)]
          (is (every? number?
                      (let [f (fn [& args] (genome-outputs gm args))]
                        (f 1 2 3))) "Evaluates to numbers"))))))
