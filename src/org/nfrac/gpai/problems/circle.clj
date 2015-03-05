(ns org.nfrac.gpai.problems.circle)

(def inputs '[r y x])

(defn actual-fn
  [^double r ^double y ^double x]
  (<= (+ (* x x) (* y y))
      (* r r)))

(defn grid-inputs
  "Generate an set of input cases with the given circle radii rs
   at regular integer points on a square grid 2*s along each side."
  [s rs]
  (for [r rs
        y (range (- s) (inc s))
        x (range (- s) (inc s))]
    [r y x]))

(defn rand-inputs
  "Generate a list of input cases with the given circle radii rs
   at 50 random points on a square grid 2*s along each side."
  [s rs]
  (let [rand-s #(- (rand (* 2 s)) s)
        n 50]
    (for [r rs
          i (range n)]
      [r (rand-s) (rand-s)])))

(defn fitness-fn
  [inputs f]
  (/ (count (filter (fn [[r y x]]
                      (= (f r y x)
                         (actual-fn r y x)))
                    inputs))
     (count inputs)))

(defn print-solution
  [s rs f]
  (let [inputs (grid-inputs s rs)
        dat (map (fn [[r y x]] {:r r, :y y, :x x
                               :actual (actual-fn r y x)
                               :output (f r y x)})
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
        (println (apply str (format "%+3d" (long y)) ": "
                        (interpose " " (map hitchar y-dat))))))))
