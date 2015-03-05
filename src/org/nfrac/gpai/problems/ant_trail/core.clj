(ns org.nfrac.gpai.problems.ant-trail.core
  "Koza's Santa Fe Ant Trail problem, common data and functions.")

(def santafe-string
  "Santa Fe trail, a 32x32 grid, from
   http://ccl.northwestern.edu/netlogo/models/community/Santa%20Fe%20Ant%20Trail
   The Santa Fe Trail consists of 144 squares with 21 turns, and the
   89 units of food are distributed non-uniformly along it. The Ant
   starts in the upper left cell of the grid (0, 0) facing east."
"
.111............................
...1............................
...1....................011100..
...1....................1....1..
...1....................1....1..
...1111011111.......01100....0..
............1.......0........1..
............1.......1........0..
............1.......1........0..
............1.......1........1..
............0.......1........0..
............1.......0........0..
............1.......0........1..
............1.......1........0..
............1.......1..0001110..
............0...01000..1........
............0...0......0........
............1...0......0........
............1...1......01000....
............1...1..........1....
............1...1..........0....
............1...1..........0....
............1...0......00010....
............1...0......1........
.001100111110...1...............
.1..............1...............
.1..............1...............
.1.....0111111100...............
.1.....1........................
.0.....1........................
.0111100........................
................................")

(def santafe-food
  "Food locations as a set of [x y] coordinates."
  (->> santafe-string
       (remove #{\newline})
       (map-indexed (fn [i c]
                      (when (= \1 c)
                        [(rem i 32) (quot i 32)])))
       (remove nil?)
       (set)))

(def start-loc [0 0])
(def start-dir 0)
(def rev-start-loc [23 24])
(def rev-start-dir 1)

(def max-steps 600)

(defn ahead-loc
  "Returns the location coordinates one move ahead of the current
   location [x y] with given direction integer:
       1(N)
   2(W)    0(E)
       3(S)."
  [[x y] dir]
  (let [[dx dy] (case dir
                  0 [1 0]   ;; east
                  1 [0 -1]  ;; north
                  2 [-1 0]  ;; west
                  3 [0 1])] ;; south
    [(+ x dx)
     (+ y dy)]))

(defn left
  "Update direction integer by turning left."
  [dir]
  (mod (inc dir) 4))

(defn right
  "Update direction integer by turning right."
  [dir]
  (mod (dec dir) 4))

(defn print-ant-trail
  "Print ant path and food, both collections of locs, on a 32x32
   grid."
  [path food]
  (println "Key: 1=eaten-food $=missed-food 0=ant-trail")
  (let [path (set path)
        food (set food)]
   (doseq [y (range 32)
           x (range 32)
           :let [food? (contains? food [x y])
                 path? (contains? path [x y])]]
     (when (zero? x) (print \newline))
     (print (cond
             (and food? path?) \1
             food? \$
             path? \0
             :else \.)))
   (print \newline)))
