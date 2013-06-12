(ns gpai.tree-test
  (:use clojure.test
        gpai.tree)
  (:require [gpai.lang.arith :as arith]
            [gpai.lang.core :as lang]))

(deftest expressions-test
  (let [fs arith/funcset
        fm (map (juxt identity lang/arity) fs)
        ts '[a b c 0]]
    (testing "Generate random expressions."
      (is (let [t (gen-terminal ts)]
            (or (number? t) (some #{t} ts))) "Terminal in set or ERC")
      (is (seq (gen-expr fm ts)) "Gen expr gives a list"))
    (testing "Modify expressions."
      (is (seq (mutate-subtree fm ts (gen-expr fm ts)))
          "Mutate")
      (is (seq (crossover-subtrees (gen-expr fm ts)
                                   (gen-expr fm ts)))
          "Crossover"))))

(deftest eval-test
  (let [fs arith/funcset
        fm (map (juxt identity lang/arity) fs)
        ts '[a b c 0]]
    (testing "Eval generated expressions as a function"
      (is (function? (lang/fn-from-expr '[a b c] (gen-expr fm ts)))
          "Generate function")
      (is (number?
           (let [f (lang/fn-from-expr '[a b c] (gen-expr fm ts))]
             (or (f 1 2 3) 0))) "Evaluates to a number or nil"))))
