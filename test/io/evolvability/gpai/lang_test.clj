(ns io.evolvability.gpai.lang-test
  (:use clojure.test)
  (:require (io.evolvability.gpai [lang-float :as f]
                                  [lang-integer :as i])))

(deftest lang-integer-test
  (testing "Integer arithmetic functions."
    (is (= 1 (i/_abs_ -1)) "Abs")
    (is (= -5 (i/_+_ -4 -1)) "Plus")
    (is (= 1 (i/_quot_ 3 2)) "Quot")
    (is (= 1 (i/_quot_ 5 0)) "Quot zero gives 1")
    (is (= 1 (i/_mod_ 3 2)) "Mod")
    (is (= 1 (i/_mod_ 3 0)) "Mod zero gives 1")
    (is (= -5 (i/_min_ -3 -5)) "Min")
    (is (= -3 (i/_max_ -3 -5)) "Max"))
  (testing "Control functions."
    (is (= :true (i/_if<_ 0 1 :true :false)) "If< true branch")
    (is (= :false (i/_if<_ 1 0 :true :false)) "If< false branch")
    (is (= :false (i/_if<_ 1 1 :true :false)) "If< strict comparison")))

(deftest lang-float-test
  (testing "Floating-point arithmetic functions."
    (is (= 1.00 (f/_abs_ -1.0)) "Abs")
    (is (= -5.00 (f/_+_ -4.0 -1.0)) "Plus")
    (is (= 0.5 (f/_div_ 1.0 2.0)) "Div")
    (is (= 1.0 (f/_div_ 5.0 0.0)) "Div zero gives 1")
    (is (= 1.0 (f/_mod_ 3.0 2.0)) "Mod")
    (is (= 1.0 (f/_mod_ 3.0 0.0)) "Mod zero gives 1")
    (is (= -5.0 (f/_min_ -3.0 -5.0)) "Min")
    (is (= -3.0 (f/_max_ -3.0 -5.0)) "Max"))
  (testing "Control functions."
    (is (= :true (f/_if<_ 0.0 1.0 :true :false)) "If< true branch")
    (is (= :false (f/_if<_ 1.0 0.0 :true :false)) "If< false branch")
    (is (= :false (f/_if<_ 1.0 1.0 :true :false)) "If< strict comparison")))

(comment
  ;; variants of functions supporting nils
  (is (nil? (i/_abs_ nil)) "Abs nil")
  (is (nil? (i/_+_ 1 nil)) "Plus nil")
  (is (nil? (i/_quot_ 5 nil)) "Quot nil")
  (is (nil? (i/_mod_ nil 0)) "Mod nil")
  (is (nil? (i/_min_ nil 0)) "Min nil")
  (is (= :true (i/_if<_ -1 nil :true :false)) "If< nil compared as 0")

  (is (nil? (f/_abs_ nil)) "Abs nil")
  (is (nil? (f/_+_ 1.0 nil)) "Plus nil")
  (is (nil? (f/_div_ 5.0 nil)) "Div nil")
  (is (nil? (f/_mod_ nil 0.0)) "Mod nil")
  (is (nil? (f/_min_ nil 0.0)) "Min nil")
  (is (= :true (f/_if<_ -1.0 nil :true :false)) "If< nil compared as 0")
  )
