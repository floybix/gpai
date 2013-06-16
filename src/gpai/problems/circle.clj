(ns gpai.problems.circle)

(def inputs '[r y x])

(defn actual-fn
  [[r y x]]
  (<= (+ (* x x) (* y y))
      (* r r)))

(def test-inputs
  (for [r [1 2.5 3.5]
        y (range -4 (inc 4))
        x (range -4 (inc 4))]
    [r y x]))

(def test-actual
  (map actual-fn test-inputs))

(defn fitness-fn
  [f]
  (let [test-out (map f test-inputs)]
    (/ (count (filter true? (map = test-out test-actual)))
       (count test-inputs))))

(defn print-solution
  [f]
  (let [dat (map (fn [[r y x]] {:r r, :y y, :x x
                               :actual (actual-fn [r y x])
                               :output (f [r y x])})
                 test-inputs)
        hitchar (fn [{:keys [output actual]}]
                  (condp = [output actual]
                    [true true] \O
                    [false true] \x
                    [true false] \?
                    [false false] \_))]
    (println "O=true positive;" "x=false negative;" "?=false positive")
    (doseq [r-dat (partition-by :r dat)]
      (println)
      (println "r =" (:r (first r-dat)))
      (doseq [y-dat (partition-by :y r-dat)
              :let [y (:y (first y-dat))]]
        (println (apply str (format "%+3d" y) ": "
                        (interpose " " (map hitchar y-dat))))))))
