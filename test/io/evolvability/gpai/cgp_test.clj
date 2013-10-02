(ns io.evolvability.gpai.cgp-test
  (:use clojure.test)
  (:require (io.evolvability.gpai [cgp :refer :all]
                                  [cgp-viz :as cgp-viz]
                                  [lang-float :as langf])))

(deftest graph-test
  (let [lang (conj langf/lang [0.0])
        inm ["a" "b" "c"]]
    (testing "Can generate random graphs."
      (is (vector? (:in (rand-node 5 lang {}))) "Random node has inputs")
      (let [gm (rand-genome inm 16 2 lang {})]
        (is (vector? (:nodes gm)) "Random genome has nodes")
        (is (= 16 (count (:nodes gm))) "Size of genome as given")))
    (testing "Calculation of active nodes"
      (let [gm (rand-genome inm 16 2 lang {})]
        (is (every? number? (active-ids gm)) "Active list contains numbers")
        (is (re-seq #" -> " (with-out-str (cgp-viz/print-active-nodes gm)))
            "Print active nodes in directed dot format")))
    (testing "Modify genomes"
      (let [gm (rand-genome inm 16 2 lang {})]
        (is (vector? (:nodes (mutate gm))) "Mutate")))))

(def gm1
  '{:inputs ["a" "b" "c"],
    :nodes [{}
            {}
            {}
            {:fn clojure.core/min, :in [1 2]}
            {:fn nil, :in [], :value 6.38965609827303}
            {:fn io.evolvability.gpai.lang-float/_mod_, :in [4 2]}
            {:fn nil, :in [], :value 5.79469347837637}
            {:fn io.evolvability.gpai.lang-float/_-_, :in [1 0]}
            {:fn io.evolvability.gpai.lang-float/_mod_, :in [3 5]}
            {:fn nil, :in [], :value 5.035921772704431}
            {:fn io.evolvability.gpai.lang-float/_-_, :in [2 2]}
            {:fn io.evolvability.gpai.lang-float/_-_, :in [3 5]}
            {:fn io.evolvability.gpai.lang-float/_-_, :in [7 6]}
            {:fn io.evolvability.gpai.lang-float/_div_, :in [2 4]}
            {:fn io.evolvability.gpai.lang-float/_-_, :in [10 10]}
            {:fn io.evolvability.gpai.lang-float/_+_, :in [6 13]}],
    :out-ids [14 15]})

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
  (let [lang (conj langf/lang [0.0])
        inm ["a" "b" "c"]]
    (let [gm (rand-genome inm 16 2 lang {})]
      (is (= gm1-expr (genome->expr gm1))
          "Compiled genome expression matches static example")
      (is (== 6.264202314823895
              (second (let [f (function gm1)] (f 1 2 3))))))))
