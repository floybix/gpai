(ns io.evolvability.gpai.problems.ant-trail.procedural
  "Koza's Santa Fe Ant Trail problem, in the original procedural
   formulation. Inspired by the implementation in fungp:
   https://github.com/probabilityZero/fungp/blob/7f49d0379f/src/fungp/sample/compile_ants.clj

   Takes a control function and a set of food locations. Runs a
   simulated ant on the grid, with each evaluation updating the ant's
   location, direction and food counter by calling the side-effecting
   functions:

   * `move!` move ahead one space and eat any food there.
   * `left!` turn left.
   * `right!` turn right.

   The control function itself takes no arguments.
   Other operators suggested are:

   * `if-food-ahead`
   * `do`, in 2 and 3 arity forms.

   When the ant moves onto a food location it eats it. The amount of
   food eaten after taking 600 actions is the fitness score."
  (:require [io.evolvability.gpai.problems.ant-trail.core :as ant]))

(def lang [[`if-food-ahead 2]
           [`do 2]
           [`do 3]
           [`(move!)]
           [`(left!)]
           [`(right!)]])

(def ^:dynamic *game-state*)

(defn init-game-state
  [food init-loc init-dir]
  {:loc init-loc
   :dir init-dir
   :food food
   :eaten 0
   :actions 0
   :path [init-loc]})

(defn- ahead-loc []
  (ant/ahead-loc (:loc *game-state*)
                 (:dir *game-state*)))

(defn food-ahead? []
  (contains? (:food *game-state*) (ahead-loc)))

(defmacro if-food-ahead
  [a b]
  `(if (food-ahead?) ~a ~b))

(defn move!
  "Move the ant ahead one space, updating *game-state*."
  []
  ;; limit actions here because each step (function call) can do many
  ;; actions. (otherwise a long program could cheat on its final call)
  (when (<= (:actions *game-state*) ant/max-steps)
    (let [nloc (ahead-loc)
          eat? (food-ahead?)]
      (set! *game-state*
            (-> *game-state*
                (assoc :loc nloc)
                (update-in [:path] conj nloc)
                (update-in [:food] disj nloc)
                (update-in [:eaten] + (if eat? 1 0))
                (update-in [:actions] inc))))))

(defn- turn!
  [delta]
  (set! *game-state*
        (-> *game-state*
            (update-in [:dir] #(mod (+ % delta) 4))
            (update-in [:actions] inc))))

(defn left!
  "Turn ant left by one compass point, updating *game-state*."
  []
  (turn! 1))

(defn right!
  "Turn ant right by one compass point, updating *game-state*"
  []
  (turn! -1))

(defn run-ant
  "Takes a procedural control function and a set of food locations,
   runs program to completion (either out of food or out of time), and
   returns the final state.

   As this function establishes a thread-local binding of var
   *game-state*, it needs to be run in a thread, separate from the
   root binding."
  [f! food init-loc init-dir & {:keys [watch]
                                :or {watch identity}}]
  (binding [*game-state* (init-game-state food init-loc init-dir)]
    (loop [t 1]
      (f!)
      (watch *game-state*)
      (let [{:keys [food actions]} *game-state*]
        (if (or (>= t ant/max-steps)
                (>= actions ant/max-steps)
                (empty? food))
          *game-state*
          (recur (inc t)))))))

(defn fitness
  [{:keys [eaten food actions]}]
  (let [time-bonus (if (empty? food)
                     (- ant/max-steps actions)
                     0)]
    (+ eaten time-bonus)))


;; koza-solution
;;
;; ifelse food-ahead
;;   [move]
;;   [
;;     turn-left
;;     ifelse food-ahead
;;       [move]
;;       [turn-right]
;;     turn-right
;;     turn-left
;;     turn-right
;;     ifelse food-ahead
;;       [move]
;;       [turn-left]
;;      move
;;   ]
