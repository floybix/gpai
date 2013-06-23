(ns gpai.tree
  "Tree-structured programs.
   i.e. classical GP."
  (:use gpai.core)
  (:require [clojure.zip :as zip]))

(def ^:dynamic *funcmap*)
(def ^:dynamic *terminals*)
(def ^:dynamic *terminal-probability* 0.5)
(def ^:dynamic *erc-probability* 0.2)
(def ^:dynamic *erc-range* [0.0 10.0])
(def ^:dynamic *max-expr-depth* 8)

(defn gen-terminal
  "ERCs are generated according to *erc-probability* and *erc-range*.
   Otherwise terminals are chosen from *terminals*."
  []
  (if (< (rand) *erc-probability*)
    (let [[a b] *erc-range*] (+ a (rand (- b a))))
    (rand-nth *terminals*)))

(defn gen-expr
  "Generate a random expression sub-tree. We don't allow terminals at
  the tree root: partly to avoid degenerate cases, also we want to
  use (meta) and can't on raw numbers. Respects *max-expr-depth*."
  ([]
     (gen-expr 0))
  ([from-depth]
     (if (and (> from-depth 0) ;; terminals not allowed at root
              (or (>= from-depth *max-expr-depth*) ;; limit depth
                  (< (rand) *terminal-probability*)))
       (gen-terminal)
       (let [[f n] (rand-nth (seq *funcmap*))]
         (list* f (repeatedly n #(gen-expr (inc from-depth))))))))

(defn expr-replacement-loc
  "Takes a (nested) expression and randomly selects a location that
   can be arbitrarily replaced, i.e. any location except for
   function position. Returns a zipper at the selected location."
  [expr]
  (let [z (zip/seq-zip (seq expr))
        locs (take-while (comp not zip/end?)
                         (iterate zip/next z))
        loc (rand-nth locs)
        ;; check for function position; can not put a terminal there!
        funcpos? (and (empty? (zip/lefts loc))
                      (seq (zip/path loc)))]
    (if funcpos? (zip/up loc) loc)))

(defn mutate-subtree
  "Replaces a randomly selected subtree with a newly generated one."
  [expr]
  (let [loc (expr-replacement-loc expr)
        depth (count (zip/path loc))]
    (-> loc
        (zip/replace (gen-expr depth))
        (zip/root))))

(defn trim
  "Cut off any expressions deeper than *max-expr-depth* by inserting
  terminals."
  [expr]
  (loop [loc (zip/seq-zip (seq expr))]
    (if (zip/end? loc)
      (zip/root loc)
      (let [depth (count (zip/path loc))]
        (if (and (>= depth *max-expr-depth*)
                 (sequential? (zip/node loc)))
          (recur (-> loc
                     (zip/replace (gen-terminal))
                     (zip/next)))
          (recur (zip/next loc)))))))

(defn maxdepth
  [expr]
  (->> (zip/seq-zip (seq expr))
       (iterate zip/next)
       (take-while (comp not zip/end?))
       (map (comp count zip/path))
       (reduce max)))

(defn crossover-subtrees
  "Takes two expressions and swaps a randomly selected subtree between
  them."
  [e1 e2]
  (let [loc1 (expr-replacement-loc e1)
        loc2 (expr-replacement-loc e2)
        new1 (-> loc1
                 (zip/replace (zip/node loc2))
                 (zip/root))
        new2 (-> loc2
                 (zip/replace (zip/node loc1))
                 (zip/root))
        ;; disallow single terminal at root
        new (if (sequential? new1) new1 new2)]
    (trim new)))

(defn print-codesizes
  [pop i]
  (let [szs (map (comp count flatten) pop)
        szo (sort szs)
        sz-max (last szo)
        sz-min (first szo)
        sz-med (median szo)]
    (println (format "Gen %d: codesizes [ %8d  %8d  %8d ]"
                     i sz-min sz-med sz-max))))
