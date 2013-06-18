(ns gpai.problems.circle)

(def inputs '[r y x])

(defn actual-fn
  [[r y x]]
  (<= (+ (* x x) (* y y))
      (* r r)))

(defn gen-inputs
  "Generate a list of input cases with the given circle radiuses 'r'
   on a square grid 2n+1 along each side."
  [rs n]
  (for [r rs
        y (range (- n) (inc n))
        x (range (- n) (inc n))]
    [r y x]))

(defn fitness-fn
  [inputs f]
  (/ (count (filter #(= (f %) (actual-fn %))
                    inputs))
     (count inputs)))

(defn print-solution
  [inputs f]
  (let [dat (map (fn [[r y x]] {:r r, :y y, :x x
                               :actual (actual-fn [r y x])
                               :output (f [r y x])})
                 inputs)
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
