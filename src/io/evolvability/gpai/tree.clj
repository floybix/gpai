(ns io.evolvability.gpai.tree
  "Tree-structured programs.
   i.e. classical GP.

   A genotype is a map of the form
   `{:expr (), :inputs [], :lang [], :options {}}`.

   The `:expr` is a lisp expression.

   The `:inputs` vector gives the inputs required for evaluation, as
   symbols. These are used as terminals for generating expressions,
   together with Ephemeral Random Constants.

   The `:lang` vector contains the available functions and constants.
   Each element must itself be a vector, with functions given as
   [fn-symbol arity], and constants as [value nil] or just [value].

   The `:options` map can hold parameters passed on to generation and
   mutation functions:

   * :max-expr-depth (default 8) maximum expression depth.
   * :terminal-prob (default 0.5) point probability of generating a
     terminal as opposed to a function call expression.
   * :erc-prob point probability of generating an Ephemeral Random
     Constant (ERC) as opposed to an input symbol given that we are
     generating a terminal (default 0.0).
   * :erc-gen (default #(* (gen/double) 10.0)) a function of no
     arguments to generate an ERC."
  (:require [io.evolvability.gpai.utils :as utils]
            [clojure.data.generators :as gen]
            [clojure.zip :as zip]
            [clojure.walk :as walk]))

(defn gen-terminal
  "ERCs are generated according to :erc-prob by calling :erc-gen.
   Otherwise terminal values are chosen from `terminals`."
  [terminals {:as options
              :keys [erc-prob erc-gen]
              :or {erc-prob 0.0
                   erc-gen #(* (gen/double) 10.0)}}]
  (if (< (gen/double) erc-prob)
    (erc-gen)
    (gen/rand-nth terminals)))

(defn gen-expr
  "Generate a random expression sub-tree. Terminals occur according to
   option :terminal-prob. We don't allow terminals at the tree root,
   to avoid degenerate cases. Respects option :max-expr-depth."
  ([funcs terminals options]
     (gen-expr funcs terminals 0 options))
  ([funcs terminals from-depth {:as options
                                :keys [terminal-prob max-expr-depth]
                                :or {terminal-prob 0.5
                                     max-expr-depth 8}}]
     (if (and (> from-depth 0) ;; terminals not allowed at root
              (or (>= from-depth max-expr-depth) ;; limit depth
                  (< (gen/double) terminal-prob)))
       (gen-terminal terminals options)
       (let [[f n] (gen/rand-nth (seq funcs))
             gen-next-expr #(gen-expr funcs terminals (inc from-depth)
                                      options)]
         (list* f (repeatedly n gen-next-expr))))))

(defn rand-genome
  "Generate a genome with a random expression."
  [inputs lang options]
  ;; split lang into constants and functions
  (let [funcs (filter second lang)
        consts (map first (remove second lang))
        terminals (concat inputs consts)]
    {:expr (gen-expr funcs terminals options)
     :inputs (vec inputs)
     :lang lang
     :options options
     :terminals terminals
     :funcs funcs}))

(defn expr-replacement-loc
  "Takes a (nested) expression and randomly selects a location that
   can be arbitrarily replaced, i.e. any location except for
   function position. Returns a zipper at the selected location."
  [expr]
  (let [z (zip/seq-zip (seq expr))
        locs (take-while (comp not zip/end?)
                         (iterate zip/next z))
        loc (gen/rand-nth locs)
        ;; check for function position; can not put a terminal there!
        funcpos? (and (empty? (zip/lefts loc))
                      (seq (zip/path loc)))]
    (if funcpos? (zip/up loc) loc)))

(defn mutate-subtree
  "Replaces a randomly selected subtree with a newly generated one."
  [{:as gm :keys [expr funcs terminals options]}]
  (let [loc (expr-replacement-loc expr)
        depth (count (zip/path loc))
        nexpr (-> loc
                  (zip/replace (gen-expr funcs terminals depth options))
                  (zip/root))]
    (assoc gm :expr nexpr)))

(defn- expr-trim
  [expr terminals options max-expr-depth]
  (loop [loc (zip/seq-zip (seq expr))]
    (if (zip/end? loc)
      (zip/root loc)
      (let [depth (count (zip/path loc))]
        (if (and (>= depth max-expr-depth)
                 (sequential? (zip/node loc)))
          (recur (-> loc
                     (zip/replace (gen-terminal terminals options))
                     (zip/next)))
          (recur (zip/next loc)))))))

(defn trim
  "Cut off any expressions deeper than option :max-expr-depth by
   inserting terminals."
  [{:as gm :keys [expr terminals options]}]
  (let [max-depth (or (:max-expr-depth options) 8)
        nexpr (expr-trim expr terminals options max-depth)]
    (assoc gm :expr nexpr)))

(defn- expr-crossover-subtrees
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
                 (zip/root))]
    ;; disallow single terminal at root
    (if (sequential? new1) new1 new2)))

(defn crossover-subtrees
  "Takes two genomes and swaps a randomly selected subtree between
   their expressions. The combined expression is trimmed according to
   option :max-expr-depth."
  [gm1 gm2]
  (let [nexpr (expr-crossover-subtrees (:expr gm1)
                                       (:expr gm2))]
    (trim (assoc gm1 :expr nexpr))))

(defn genome->expr
  "Converts a genome into a quoted function expression."
  [{:as gm :keys [expr inputs]}]
  `(fn ~inputs ~expr))

(defn function
  "Converts a genome into a function, using `eval`. Assumes that all
   lang symbols are fully qualified."
  [gm]
  (eval (genome->expr gm)))

(defn expr-depth
  "Returns the maximum depth of nesting in the expression."
  [expr]
  (->> (zip/seq-zip (seq expr))
       (iterate zip/next)
       (take-while (comp not zip/end?))
       (map (comp count zip/path))
       (reduce max)))

(defn print-codesizes
  [i xs _]
  (let [szs (map (comp count flatten :expr) xs)
        szo (sort szs)
        sz-max (last szo)
        sz-min (first szo)
        sz-med (utils/median szo)]
    (println (format "Gen %d: codesizes [ %8d  %8d  %8d ]"
                     i sz-min sz-med sz-max))))
