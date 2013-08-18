(ns io.evolvability.gpai.tree-test
  (:use clojure.test)
  (:require (io.evolvability.gpai [tree :refer :all]
                                  [lang-arith :as arith]
                                  [utils :refer [arity]])))

(deftest expressions-test
  (let [fs (conj arith/funcset-real 0)
        lang (map (juxt identity arity) fs)
        ins '[a b c]]
    (testing "Generate random expressions."
      (is (let [t (gen-terminal ins {})]
            (or (number? t) (some #{t} ins))) "Terminal in set or ERC")
      (is (seq (gen-expr lang ins {})) "Gen expr gives a list"))
    (testing "Modify expressions."
      (let [gm (rand-genome ins lang {})]
        (is (seq (:expr (mutate-subtree gm))) "Mutate")
        (is (seq (:expr (crossover-subtrees gm gm))) "Crossover")))))

(deftest eval-test
  (let [fs (conj arith/funcset-real 0)
        lang (map (juxt identity arity) fs)
        ins '[a b c]]
    (testing "Eval generated expressions as a function"
      (let [f (genome->fn (rand-genome ins lang {}))]
        (is (function? f) "Generate function")
        (is (number? (or (f 1 2 3) 0)) "Evaluates to a number or nil")))))
