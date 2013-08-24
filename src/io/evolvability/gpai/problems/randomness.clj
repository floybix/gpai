(ns io.evolvability.gpai.problems.randomness
  "A zero-sum game of mimicry vs discrimination - for co-evolution.

   The Generator population are to generate a sequence of integer
   values. The opposing Discriminator population are to distinguish
   the Generators' sequences from others derived from an actual Random
   Number Generator.

   A duel is run as follows. The Generator makes up 32 sequences, each
   of length 32, of integers in the range 0 to 1024 (*magnitude*). The
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
   a new integer seed (in that order). This function will be iterated
   to produce an output sequence. Output integers are transformed into
   the valid range by taking their absolute value and modulus by 1024.

   A Discriminator function also works one element at a time. It runs
   in a `reduce` operation over the input integer sequence. A further
   6 integers are allowed for carry-over state. I.e. the function
   takes 7 arguments. It returns 6 integers of carry-over state, plus
   a 7th output integer which is taken as the final output on the
   last iteration; otherwise it is ignored. The state arguments are
   initially 0."
  (:require [clojure.data.generators :as gen]))

(def ^:dynamic *magnitude* 1024)

(def discriminator-inputs '[x i1 i2 i3 i4 i5 i6])

(def generator-inputs '[seed])

(defn- abs [x] (if (neg? x) (unchecked-negate (long x)) x))

(defn _nth-bit_
  "Tests bit at binary position `(abs n)` of integer `(abs x)`
   returning 1 or 0."
  [x n]
  (if (bit-test (abs x) (abs n))
    1 0))

(defn rand-seq
  "Return a sequence of `n` uniform random integers within
   `*magnitude*` given a random seed."
  [n seed]
  (binding [gen/*rnd* (java.util.Random. seed)]
    (repeatedly n #(gen/uniform 0 *magnitude*))))

(defn nonrandomness-score
  "Iterate the discriminator function over the integer sequence `xs`
   producing a single integer score."
  [discriminator xs]
  (last (reduce (fn [inputs x]
                  (let [state (butlast inputs)
                        ans (apply discriminator x state)]
                    (vec ans)))
                (repeat (count discriminator-inputs) 0)
                xs)))

(defn gen-seq
  "Iterate the generator function for an output sequence of length
   `length`, taking initial seed `seed`."
  [generator length seed]
  (loop [out []
         z seed
         i length]
    (if (zero? i)
      out
      (let [[o nz] (generator z)
            o-ok (mod (abs o) *magnitude*)]
        (recur (conj out o-ok) nz (dec i))))))

(defn duel
  "Takes a generator function and a discriminator function. Generator
   makes `n-seq` sequences each of length `length`, seeded with a
   range of integers, and the same are also produced by a random
   number generator. The discriminator is called with each sequence.
   Fitness values are returned as [generator-fitness
   discriminator-fitness], being the counts of incorrect vs correct
   classifications on pairs of sequences."
  [n-seq length generator discriminator]
  (let [gs (map (partial gen-seq generator length) (range n-seq))
        rs (map (partial rand-seq length) (range n-seq)) ;; memoize?
        gos (map (partial nonrandomness-score discriminator) gs)
        ros (map (partial nonrandomness-score discriminator) rs)
        discores (map (fn [go ro] (cond
                                  (> go ro) 1
                                  (< go ro) -1
                                  :else 0))
                      gos ros)
        discore (reduce + discores)]
    ;; return [generator-fitness discriminator-fitness]
    [(- discore) discore]))
