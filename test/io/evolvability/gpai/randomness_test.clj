(ns io.evolvability.gpai.randomness-test
  (:use clojure.test)
  (:require [io.evolvability.gpai.problems.randomness :as rness]))

(defn- abs [x] (if (neg? x) (- x) x))

(deftest randomness-test
  (testing "Randomness problem support functions."
    (is (every? symbol? rness/discriminator-inputs))
    (is (= 1 (rness/_nth-bit_ 1 0)))
    (is (= 0 (rness/_nth-bit_ 1 1)))
    (is (= 10 (count (rness/rand-seq 10 1))))
    (is (every? #(< -1 % rness/*magnitude*) (rness/rand-seq 100 1)))
    (let [gen-f (fn [seed]
                  [(* 2 seed) (inc seed)])
          disc-f (fn [x i1 i2 i3 i4 i5 i6]
                   (let [n1 x ;; store last value
                         n2 (+ i2 (if (< (abs (- x i1)) 5)
                                    1
                                    0))
                         n3 (- x i1) ;; store last difference
                         n4 (+ i4 (if (= n3 i3)
                                    1
                                    0))
                         ;; add accumulated scores as non-randomness score
                         out (+ n2 n4)]
                     [n1 n2 n3 n4 0 0 out]))]
      (is (= 32 (count (rness/gen-seq gen-f 32 1))))
      (is (= (vec (rness/gen-seq gen-f 5 0))
             [0 2 4 6 8]))
      (is (= (rness/nonrandomness-score disc-f (rness/gen-seq gen-f 5 0))
             9))
      (is (= (rness/duel 32 32 gen-f disc-f)
             [-32 32])))))
