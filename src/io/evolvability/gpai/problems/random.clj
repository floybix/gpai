(ns io.evolvability.gpai.problems.random
  "A zero-sum game of mimicry vs discrimination - for co-evolution.

   The Generator population are to generate a sequence of integer
   values. The opposing Discriminator population are to distinguish
   the Generators' sequences from others derived from an actual Random
   Number Generator.

   A duel is run as follows. The Generator makes up 32 sequences, each
   of length 32, of integers in the range 0 to 1024 (2^10). The
   Generator is passed a seed for each sequence starting at 0 and
   incrementing by 1. A similar set of a sequences are built by a
   Random Number Generator. The Discriminator is then presented with
   these one at a time and responds with numeric scores. The higher
   score from evaluating a pair of sequences indicates the one
   determined as non-random (a Generator's). The number of correct
   choices defines the Discriminator's fitness value, and the number
   of incorrect choices defines the Generator's fitness value.

   A Generator function produces one element at a time. It takes as
   input a single integer seed, and returns both an output integer and
   a new integer seed. This function will be iterated to produce an
   output sequence. Output integers are transformed into the valid
   range by taking their absolute value and modulus by 1024.

   A Discriminator function also works one element at a time. It runs
   in a `reduce` operation over the input integer sequence. A further
   4 integers are allowed for carry-over state. I.e. the function
   takes 5 arguments and returns 4 integers of carry-over state;
   lastly a 5th output integer is taken as the final output on the
   last iteration; otherwise it is ignored. The state arguments are
   initially 0."
  (:require [clojure.data.generators :as gen]))

(def discriminator-inputs '[x a b c d])

(def generator-inputs '[seed])

(defn- abs [x] (if (neg? x) (- x) x))

(defn _bit-test_
  [x n]
  (if (bit-test (abs x) (abs n))
    1 0))

(defn rand-seq
  [n seed]
  (binding [gen/*rnd* (java.util.Random. seed)]
    (repeatedly n (gen/uniform 0 1024))))

(defn seq-nonrandom-score
  [discriminator xs]
  (last (reduce (fn [[a b c d out] x]
                  (let [state (discriminator x a b c d)]
                    state))
                [0 0 0 0 0]
                xs)))

(defn gen-seq
  [generator length seed]
  (loop [out (vec)
         z seed
         i length]
    (if (zero? i)
      out
      (let [[o nz] (generator z)
            o-ok (mod (abs o) 1024)]
        (recur (conj out o-ok) nz (dec i))))))

(defn duel
  [n-seq length generator discriminator]
  (let [gs (map (partial gen-seq generator length) (range n-seq))
        rs (map (partial rand-seq length) (range n-seq)) ;; memoize?
        gos (map (partial seq-nonrandom-score discriminator) gs)
        ros (map (partial seq-nonrandom-score discriminator) rs)
        correct (map (fn [go ro] (> go ro))
                     gos ros)]
    ;; return [generator-fitness discriminator-fitness]
    [(count (filter not correct))
     (count (filter identity correct))]))
