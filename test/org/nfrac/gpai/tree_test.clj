(ns org.nfrac.gpai.tree-test
  (:use clojure.test)
  (:require (org.nfrac.gpai [tree :refer :all]
                            [lang-float :as langf])))

(deftest expressions-test
  (let [lang (conj langf/lang [0.0])
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
  (let [lang (conj langf/lang [0.0])
        ins '[a b c]]
    (testing "Eval generated expressions as a function"
      (let [f (function (rand-genome ins lang {}))]
        (is (function? f) "Generate function")
        (is (number? (f 1 2 3)) "Evaluates to a number")))))
