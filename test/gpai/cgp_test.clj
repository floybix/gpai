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

(def gm1
  '{:inputs ["a" "b" "c"],
    :genes [{}
            {}
            {}
            {:fn gpai.lang.arith/_min_, :in [2 1]}
            {:fn nil, :in [], :value 6.38965609827303}
            {:fn gpai.lang.arith/_mod_, :in [1 3]}
            {:fn nil, :in [], :value 5.79469347837637}
            {:fn gpai.lang.arith/_-_, :in [6 7]}
            {:fn gpai.lang.arith/_mod_, :in [5 3]}
            {:fn nil, :in [], :value 5.035921772704431}
            {:fn gpai.lang.arith/_-_, :in [8 8]}
            {:fn gpai.lang.arith/_-_, :in [8 6]}
            {:fn gpai.lang.arith/_-_, :in [5 6]}
            {:fn gpai.lang.arith/_div_, :in [11 9]}
            {:fn gpai.lang.arith/_-_, :in [4 4]}
            {:fn gpai.lang.arith/_+_, :in [9 2]}],
    :out-idx [14 15]})

(def gm1-expr
  '(clojure.core/fn
     [g-0_ g-1_ g-2_]
     (clojure.core/let
         [g-4_ 6.38965609827303
          g-6_ 5.79469347837637
          g-10_ (gpai.lang.arith/_-_ g-2_ g-2_)
          g-13_ (gpai.lang.arith/_div_ g-2_ g-4_)
          g-14_ (gpai.lang.arith/_-_ g-10_ g-10_)
          g-15_ (gpai.lang.arith/_+_ g-6_ g-13_)]
       [g-14_ g-15_])))

(deftest compiler-test
  (let [fs arith/funcset
        fm (map (juxt identity lang/arity) fs)
        inn ["a" "b" "c"]]
    (binding [*funcmap* fm]
      (let [gm (rand-genome inn 16 2)]
        (is (= gm1-expr (genome->expr gm1))
            "Compiled genome expression matches static example")))))
