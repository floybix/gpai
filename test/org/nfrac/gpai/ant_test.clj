(ns org.nfrac.gpai.ant-test
  (:use clojure.test)
  (:require (org.nfrac.gpai.problems.ant-trail [core :as ant]
                                               [procedural :as ant-p]
                                               [functional :as ant-f])
            [clojure.data.generators :as gen]
            [clojure.pprint :as pp]))

(deftest ant-test
  (testing "Ant trail problem core functions."
    (is (= [1 0] (ant/ahead-loc [0 0] 0)) "Ahead location east.")
    (is (= [0 1] (ant/ahead-loc [0 0] 3)) "Ahead location south.")
    (is (= 0 (ant/left 3)) "Turn from south to east."))
  (testing "Ant trail functional problem support."
    (let [f (fn [food-ahead? state-a state-b]
              [::ant-f/move 0 0])]
      (is (= 3 (:eaten (ant-f/run-ant f ant/santafe-food [0 0] 0)))
          "Constant move function eats 3 food."))))
