(ns io.evolvability.gpai.lang-test
  (:use clojure.test)
  (:require (io.evolvability.gpai [lang-float :as f]
                                  [lang-integer :as i])))

(deftest lang-integer-test
  (testing "Integer arithmetic functions."
    (is (= 1 (i/abs -1)) "Abs")
    (is (= -5 (i/_+_ -4 -1)) "Plus")
    (is (= 1 (i/_quot_ 3 2)) "Quot")
    (is (= 1 (i/_quot_ 5 0)) "Quot zero gives 1")
    (is (= 1 (i/_mod_ 3 2)) "Mod")
    (is (= 1 (i/_mod_ 3 0)) "Mod zero gives 1"))
  (testing "Control functions."
    (is (= 1 (i/if< 2 3 1 -1)) "If< true branch")
    (is (= -1 (i/if< 3 2 1 -1)) "If< false branch")
    (is (= -1 (i/if< 2 2 1 -1)) "If< strict comparison")))

(deftest lang-float-test
  (testing "Floating-point arithmetic functions."
    (is (= 1.00 (f/abs -1.0)) "Abs")
    (is (= -5.00 (f/_+_ -4.0 -1.0)) "Plus")
    (is (= 0.5 (f/_div_ 1.0 2.0)) "Div")
    (is (= 1.0 (f/_div_ 5.0 0.0)) "Div zero gives 1")
    (is (= 1.0 (f/_mod_ 3.0 2.0)) "Mod")
    (is (= 1.0 (f/_mod_ 3.0 0.0)) "Mod zero gives 1"))
  (testing "Control functions."
    (is (= 1.0 (f/if< 2.2 3.3 1.0 -1.0)) "If< true branch")
    (is (= -1.0 (f/if< 3.3 2.2 1.0 -1.0)) "If< false branch")
    (is (= -1.0 (f/if< 2.2 2.2 1.0 -1.0)) "If< strict comparison")))
