(ns org.nfrac.gpai.problems.ant-trail.functional
  "Koza's Santa Fe Ant Trail problem, adapted to a functional style.

   Runs a simulated ant on the grid, with each evaluation of the
   function deciding on what the ant will do next: move ahead, turn
   left or turn right (on a square-tiled grid).

   The decision function gives an output of type `::action`, one of:

   * `::move`
   * `::turn-left`
   * `::turn-right`

   These are made available to the decision function as constants.

   The decision function also produces additional output values which
   are used to represent state. They are passed back in as arguments
   to the next iteration of the decision function.

   So the arguments are:

   1. `food-ahead?` (`Boolean`)
   2. `state-a` (`Number`)
   3. `state-b` (`Number`)

   Operators suggested are:

   * `::action` functions: `if`
   * `Boolean` functions:  `=, and, or, not`
   * `Number` functions: `if, <, +, -, *, quot, mod`

   When the ant moves onto a food location it eats it. The amount of
   food eaten after taking 600 actions is the fitness score."
  (:require [org.nfrac.gpai.problems.ant-trail.core :as ant]
            [clojure.data.generators :as gen]))

(derive ::turn-left ::turn)
(derive ::turn-right ::turn)
(derive ::move ::action)
(derive ::turn ::action)

(def inputs [["food-ahead?" Boolean]
             ["state-a" Long]
             ["state-b" Long]])

(def outputs [::action Long Long])

(def lang-actions
  [;[`= [Boolean, ::action ::action]]
   [`if [::action, Boolean ::action ::action]]])

(def constants
  [[::move ::action]
   [::turn-left ::action]
   [::turn-right ::action]
   [0 Long]
   [1 Long]
   [2 Long]
   [3 Long]
   [4 Long]])

(defn limit-magn
  "Truncate an integer to range of Long, avoiding crazy arbitrarily
   large integers (I hit one with 2 million decimal digits)."
  [i]
  (min Long/MAX_VALUE
       (max Long/MIN_VALUE
            i)))

(defn ant-step
  "Computes one step of an ant program given a state map and returns a
   modified state map."
  [{:as state
    :keys [fun
           loc
           dir
           food
           eaten
           actions
           state-vals
           
           prev-acts
           prev-sens]
    :or {eaten 0
         actions 0
         state-vals (repeat 2 0)
         
         prev-acts (repeat 4 ::move)
         prev-sens (repeat 1 true)}}]
  (let [aloc (ant/ahead-loc loc dir)
        food-ahead? (contains? food aloc)
        [what & svals] (apply fun food-ahead? state-vals)
        ; (apply fun food-ahead? (first prev-sens) (take 4 prev-acts))
        nloc (if (= what ::move) aloc loc)
        eat? (and (= what ::move) food-ahead?)
        ndir (case what
               ::turn-left (ant/left dir)
               ::turn-right (ant/right dir)
               ::move dir)]
    (assoc state
      :loc nloc
      :dir ndir
      :food (disj food nloc)
      :eaten (+ eaten (if eat? 1 0))
      :actions (inc actions)
      :action what
      :food-ahead? food-ahead?
      :state-vals (map limit-magn svals)
      
      :prev-acts (cons what prev-acts)
      :prev-sens (cons food-ahead? prev-sens))))

(defn run-ant
  "Takes a decision function and a set of food locations, runs program
   to completion (either out of food or out of time), and returns
   the final state, including the path of locations in key :path."
  [f init-food init-loc init-dir]
  (let [init {:fun f
              :food init-food
              :loc init-loc
              :dir init-dir}
        steps (->> (iterate ant-step init)
                   (take-while #(and (seq (:food %))
                                     (<= (:actions % 0) ant/max-steps))))]
    (-> (last steps)
        (assoc :path (map :loc steps))
        (assoc :steps steps))))

(defn fitness
  [{:keys [eaten food actions]}]
  (let [time-bonus (if (empty? food)
                     (- ant/max-steps actions)
                     0)]
    (+ eaten time-bonus)))
