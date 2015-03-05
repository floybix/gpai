(ns org.nfrac.gpai.problems.randomness
  "A zero-sum game of mimicry vs discrimination, as an example of
   co-evolution.

   There are two opposing populations, the Generators and the
   Discriminators. The Generators have to generate a sequence of
   integer values. The opposing Discriminators have to distinguish the
   Generators' sequences from others derived from an actual Random
   Number Generator.

   A typical duel is run as follows. The Generator makes up 16
   sequences, each of length 16, of integers in the range 0 to
   1024 (`magnitude`). The Generator is passed a seed for each
   sequence starting at 0 and incrementing by 1. A similar set of a
   sequences are built by a Random Number Generator. The Discriminator
   is then presented with these one at a time and responds with
   numeric scores. The higher score from evaluating a pair of
   sequences indicates the one predicted as being non-random (a
   Generator's). The Discriminator's fitness value is the number of
   correct choices minus the number of incorrect choices.
   Symmetrically, the Generator's fitness value is the same negated.

   A Generator function produces one element at a time. It takes as
   input a single integer seed, and returns both an output integer and
   a new integer seed (in that order). This function will be iterated
   to produce an output sequence. Output integers are transformed into
   the valid range by taking their absolute value and modulus by 1024.

   A Discriminator function also works one element at a time. It runs
   within a `reduce` operation over the input integer sequence. A
   further 6 integers are allowed for carry-over state. I.e. the
   function takes 7 arguments. It also returns 7 integers: the first
   is taken as the function output on the final iteration (otherwise
   it is ignored) and the remaining 6 integers are carried over to the
   next iteration. These state arguments are initially 0."
  (:require [clojure.data.generators :as gen]))

;; Note this is an example only, number of inputs can be varied.
(def discriminator-inputs '[x i1 i2 i3 i4 i5 i6])

(def generator-inputs '[seed])

(defn _nth-bit_
  "Tests bit at binary position `(abs n)` of integer `(abs x)`
   returning 1 or 0."
  ^long [^long x ^long n]
  (if (bit-test (Math/abs x) (Math/abs n))
    1 0))

(defn rand-seq
  "Return a sequence of `n` uniform random integers within
   `magnitude` given a random seed."
  [n magnitude seed]
  (binding [gen/*rnd* (java.util.Random. seed)]
    (repeatedly n #(gen/uniform 0 magnitude))))

(defn nonrandomness-score
  "Iterate the discriminator function over the integer sequence `xs`
   producing a single integer score."
  [discriminator n-in xs]
  (first (reduce (fn [[out & state] x]
                   (apply discriminator x state))
                 (repeat n-in 0)
                 xs)))

(defn gen-seq
  "Iterate the generator function for an output sequence of length
   `length`, taking initial seed `seed`."
  [generator length magnitude seed]
  (loop [out []
         z seed
         i length]
    (if (zero? i)
      out
      (let [[o nz] (generator z)
            o-ok (mod (Math/abs (long o)) magnitude)]
        (recur (conj out o-ok) nz (dec i))))))

(defn duel
  "Takes a generator function and a discriminator function. Generator
   makes `n-seq` sequences each of length `length`, seeded with a
   range of integers, and the same are also produced by a random
   number generator. The discriminator is applied to each sequence
   separately. Note that the number of inputs expected by the
   discriminator must be passed in here. Fitness values are returned
   as [generator-fitness discriminator-fitness]."
  [n-seq length magnitude generator discriminator n-in]
  (let [gs (map (partial gen-seq generator length magnitude) (range n-seq))
        rs (map (partial rand-seq length magnitude) (range n-seq))
        run-disc (partial nonrandomness-score discriminator n-in)
        gos (map run-disc gs)
        ros (map run-disc rs)
        scores (map (fn [go ro] (cond
                                (> go ro) 1
                                (< go ro) -1
                                :else 0))
                    gos ros)
        score (reduce + scores)]
    ;; return [generator-fitness discriminator-fitness]
    [(- score) score]))
