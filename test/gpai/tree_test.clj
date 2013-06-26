(ns gpai.tree-test
  (:use clojure.test
        gpai.tree)
  (:require [gpai.lang.arith :as arith]
            [gpai.lang.core :as lang]))

(deftest expressions-test
  (let [fs arith/funcset-real
        fm (map (juxt identity lang/arity) fs)
        ts '[a b c 0]]
    (binding [*terminals* ts
              *funcmap* fm]
      (testing "Generate random expressions."
        (is (let [t (gen-terminal)]
             (or (number? t) (some #{t} ts))) "Terminal in set or ERC")
        (is (seq (gen-expr)) "Gen expr gives a list"))
      (testing "Modify expressions."
        (is (seq (mutate-subtree (gen-expr)))
            "Mutate")
        (is (seq (crossover-subtrees (gen-expr)
                                     (gen-expr)))
            "Crossover")))))

(deftest eval-test
  (let [fs arith/funcset-real
        fm (map (juxt identity lang/arity) fs)
        ts '[a b c 0]]
    (binding [*terminals* ts
              *funcmap* fm]
      (testing "Eval generated expressions as a function"
        (is (function? (lang/fn-from-expr '[a b c] (gen-expr)))
            "Generate function")
        (is (number?
             (let [f (lang/fn-from-expr '[a b c] (gen-expr))]
               (or (f 1 2 3) 0))) "Evaluates to a number or nil")))))
