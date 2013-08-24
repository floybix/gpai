(ns io.evolvability.gpai.cgp-test
  (:use clojure.test)
  (:require (io.evolvability.gpai [cgp :refer :all]
                                  [lang-float :as langf]
                                  [utils :refer [arity]])))

(deftest graph-test
  (let [fs langf/funcset
        lang (map (juxt identity arity) fs)
        inm ["a" "b" "c"]]
    (testing "Can generate random graphs."
      (is (vector? (:in (rand-node 5 lang {}))) "Random node has inputs")
      (let [gm (rand-genome inm 16 2 lang {})]
        (is (vector? (:nodes gm)) "Random genome has nodes")
        (is (= 16 (count (:nodes gm))) "Size of genome as given")))
    (testing "Calculation of active nodes"
      (let [gm (rand-genome inm 16 2 lang {})]
        (is (every? number? (active-idx gm)) "Active list contains numbers")
        (is (re-seq #" -> " (with-out-str (print-active-nodes gm)))
            "Print active nodes in directed dot format")))
    (testing "Modify genomes"
      (let [gm (rand-genome inm 16 2 lang {})]
        (is (vector? (:nodes (mutate gm))) "Mutate")))))

(deftest eval-test
  (let [fs langf/funcset
        lang (map (juxt identity arity) fs)
        inm ["a" "b" "c"]]
    (testing "Eval generated expressions as a function"
      (let [gm (rand-genome inm 16 2 lang {})]
        (is (every? number?
                    (let [f (fn [& args] (genome-outputs gm args))]
                      (f 1 2 3))) "Evaluates to numbers")))))

(def gm1
  '{:inputs ["a" "b" "c"],
    :nodes [{}
            {}
            {}
            {:fn io.evolvability.gpai.lang-float/_min_, :in [2 1]}
            {:fn nil, :in [], :value 6.38965609827303}
            {:fn io.evolvability.gpai.lang-float/_mod_, :in [1 3]}
            {:fn nil, :in [], :value 5.79469347837637}
            {:fn io.evolvability.gpai.lang-float/_-_, :in [6 7]}
            {:fn io.evolvability.gpai.lang-float/_mod_, :in [5 3]}
            {:fn nil, :in [], :value 5.035921772704431}
            {:fn io.evolvability.gpai.lang-float/_-_, :in [8 8]}
            {:fn io.evolvability.gpai.lang-float/_-_, :in [8 6]}
            {:fn io.evolvability.gpai.lang-float/_-_, :in [5 6]}
            {:fn io.evolvability.gpai.lang-float/_div_, :in [11 9]}
            {:fn io.evolvability.gpai.lang-float/_-_, :in [4 4]}
            {:fn io.evolvability.gpai.lang-float/_+_, :in [9 2]}],
    :out-idx [14 15]})

(def gm1-expr
  '(clojure.core/fn
     [nd-0_ nd-1_ nd-2_]
     (clojure.core/let
         [nd-4_ 6.38965609827303
          nd-6_ 5.79469347837637
          nd-10_ (io.evolvability.gpai.lang-float/_-_ nd-2_ nd-2_)
          nd-13_ (io.evolvability.gpai.lang-float/_div_ nd-2_ nd-4_)
          nd-14_ (io.evolvability.gpai.lang-float/_-_ nd-10_ nd-10_)
          nd-15_ (io.evolvability.gpai.lang-float/_+_ nd-6_ nd-13_)]
       [nd-14_ nd-15_])))

(deftest compiler-test
  (let [fs langf/funcset
        lang (map (juxt identity arity) fs)
        inm ["a" "b" "c"]]
    (let [gm (rand-genome inm 16 2 lang {})]
      (is (= gm1-expr (genome->expr gm1))
          "Compiled genome expression matches static example"))))