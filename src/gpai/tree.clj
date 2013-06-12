(ns gpai.tree
  (:use gpai.core)
  (:require [clojure.zip :as zip]))

(def ^:dynamic *terminal-probability* 0.5)
(def ^:dynamic *erc-probability* 0.2)
(def ^:dynamic *erc-range* [0.0 10.0])
(def ^:dynamic *max-expr-depth* 10)

(defn gen-terminal
  [terminals]
  (if (< (rand) *erc-probability*)
    (let [[a b] *erc-range*] (+ a (rand (- b a))))
    (rand-nth terminals)))

(defn gen-expr
  "Generate a random expression sub-tree. We don't allow terminals at
  the tree root: partly to avoid degenerate cases, also we want to
  use (meta) and can't on raw numbers. Respects *max-expr-depth*."
  ([funcmap terminals]
     (gen-expr funcmap terminals 0))
  ([funcmap terminals from-depth]
     (if (and (> from-depth 0) ;; terminals not allowed at root
              (or (>= from-depth *max-expr-depth*) ;; limit depth
                  (< (rand) *terminal-probability*)))
       (gen-terminal terminals)
       (let [[f n] (rand-nth (seq funcmap))]
         (list* f (repeatedly n #(gen-expr funcmap terminals
                                           (inc from-depth))))))))

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

;(defn trim)

(defn mutate-subtree
  "Replaces a randomly selected subtree with a newly generated one."
  [funcmap terminals expr]
  (let [loc (expr-replacement-loc expr)
        depth (count (zip/path loc))]
    (-> loc
        (zip/replace (gen-expr funcmap terminals depth))
        (zip/root))))

(defn crossover-subtrees
  "Takes two expressions and swaps a randomly selected subtree between
  them."
  [e1 e2]
  ;; TODO: trim result to max depth
  (let [loc1 (expr-replacement-loc e1)
        loc2 (expr-replacement-loc e2)
        new1 (-> loc1
                 (zip/replace (zip/node loc2))
                 (zip/root))
        new2 (-> loc2
                 (zip/replace (zip/node loc1))
                 (zip/root))]
    ;; disallow single terminal at root
    (if (sequential? new1) new1 new2)))

(defn print-codesizes
  [pop i]
  (let [szs (map (comp count flatten) pop)
        szo (sort szs)
        sz-max (last szo)
        sz-min (first szo)
        sz-med (median szo)]
    (println (format "Gen %d: codesizes [ %8d  %8d  %8d ]"
                     i sz-min sz-med sz-max))))
