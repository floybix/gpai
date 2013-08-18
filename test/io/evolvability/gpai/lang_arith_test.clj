(ns io.evolvability.gpai.lang-arith-test
  (:use clojure.test
        io.evolvability.gpai.lang-arith))

(deftest lang-arith-test
  (testing "Arithmetic functions."
    (is (== 1 (_abs_ -1)) "Abs")
    (is (nil? (_abs_ nil)) "Abs nil")
    (is (== -5 (_+_ -4 -1)) "Plus")
    (is (nil? (_+_ 1 nil)) "Plus nil")
    (is (== 0.5 (_div_ 1 2)) "Div")
    (is (== 1 (_div_ 5 0)) "Div zero gives 1")
    (is (nil? (_div_ 5 nil)) "Div nil")
    (is (== 1 (_quot_ 3 2)) "Quot")
    (is (== 1 (_quot_ 5 0)) "Quot zero gives 1")
    (is (nil? (_quot_ 5 nil)) "Quot nil")
    (is (== 1 (_mod_ 3 2)) "Mod")
    (is (== 1 (_mod_ 3 0)) "Mod zero gives 1")
    (is (nil? (_mod_ nil 0)) "Mod nil")
    (is (== -5 (_min_ -3 -5)) "Min")
    (is (nil? (_min_ nil 0)) "Min nil"))
  (testing "Control functions."
    (is (= :true (_if<_ 0 1 :true :false)) "If< true branch")
    (is (= :false (_if<_ 1 0 :true :false)) "If< false branch")
    (is (= :false (_if<_ 1 1 :true :false)) "If< strict comparison")
    (is (= :true (_if<_ -1 nil :true :false)) "If< nil compared as 0")))
