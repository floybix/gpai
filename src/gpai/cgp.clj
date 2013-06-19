(ns gpai.cgp
  "General graph-structured programs.
   i.e. CGP with 1 row and no n-back limit.
   
   A genotype is a map of the form
   `{:genes [], :inputs [], :out-idx []}`.

   The inputs vector defines the number of inputs and contains their
   names (used for display only). The `:out-idx` vector defines the
   number of outputs and contains the gene indices to use as outputs.

   Each gene is a map of the form
   `{:fn 'foo, :in [1 4 1 ...]}`

   where :fn gives the function as a symbol; the :in vector points to
   fn arguments as a number of cells back. The number of arguments
   must match the arity of the function. Some genes may be generated
   as Ephemeral Random Constants. These
   genes have :fn nil and store a :value.

  The leading genes are for inputs and have :in nil and :fn nil."
  (:require [clojure.java.shell :as sh]
            [clojure.java.browse :as br]))

(def ^{:doc "Map of functions (as namespaced symbols) to their arity."}
  ^:dynamic *funcmap*)
(def ^:dynamic *erc-probability* 0.2)
(def ^:dynamic *erc-range* [0.0 10.0])
(def ^:dynamic *gene-mut-rate* 0.1)

(defn rand-gene
  "Returns a new gene at the given offset (which constrains the
   distance back of input links). ERCs are generated according to
   *erc-probability* and *erc-range*. Otherwise functions are chosen
   from *funcmap*."
  [offset]
  (if (< (rand) *erc-probability*)
    (let [[a b] *erc-range*
          v (+ a (rand (- b a)))]
      {:fn nil :in [] :value v})
    (let [[f n] (rand-nth (seq *funcmap*))
          linkback #(inc (rand-int offset))]
      {:fn f :in (vec (repeatedly n linkback))})))

(defn rand-genome
  [inputs size n-out]
  (let [n-in (count inputs)
        in-genes (repeat n-in {})
        fn-genes (map rand-gene (range n-in size))]
    {:inputs (vec inputs)
     :genes (vec (concat in-genes fn-genes))
     :out-idx (vec (repeatedly n-out #(rand-nth (range n-in size))))}))

(defn active-idx
  "Returns the set of indices corresponding to active genes, i.e.
   those that the current output genese depend on."
  [{:keys [genes out-idx inputs]}]
  (loop [act (set out-idx)
         more (set out-idx)]
    (if-let [i (first more)]
      (if (>= i (count inputs))
        ;; function gene
        (let [g (nth genes i)
              in-idx (map (partial - i) (:in g))]
          (recur (into act in-idx)
                 (into (disj more i) in-idx)))
        ;; input gene
        (recur act (disj more i)))
      ;; done
      act)))

(defn eval-genome
  "Takes a vector of input values. Associates values with all active
  genes by calling the respective functions, storing the result in
  :value."
  [{:keys [genes out-idx inputs] :as gm} input-vals]
  (let [n-in (count inputs)
        active (sort (seq (active-idx gm)))
        in-genes (map #(hash-map :value %) input-vals)
        gs (apply assoc genes (interleave (range n-in)
                                          in-genes))]
    (loop [gs gs
           to-eval (drop-while #(< % n-in) active)]
      (if-let [i (first to-eval)]
        (let [g (nth gs i)
              in-idx (map (partial - i) (:in g))
              invals (map #(get-in gs [% :value]) in-idx)
              v (if-let [f (:fn g)]
                  (apply (resolve f) invals)
                  (:value g))]
          (recur (assoc-in gs [i :value] v)
                 (next to-eval)))
        ;; done
        gs))))

(defn genome-outputs
  "Evaluates and returns the genome outputs using given input values."
  [{:keys [genes out-idx inputs] :as gm} input-vals]
  (let [gs (eval-genome gm input-vals)]
    (map #(get-in gs [% :value]) out-idx)))

(defn print-active-genes
  "Prints the graph of active genes in DOT format."
  [{:keys [genes out-idx inputs] :as gm}]
  (let [n-in (count inputs)
        active (sort (seq (active-idx gm)))
        pr-gene (fn [i nm]
                  (println (format "g%d [label=\"%s\"];"
                                   i (str nm))))
        pr-ingene (fn [i nm]
                    (println (format "g%d [label=\"%s\",shape=box];"
                                     i (str nm))))
        pr-link (fn [i1 i2 j]
                  (println (format "g%d -> g%d [label=%d];"
                                   i1 i2 j)))]
    (println "digraph activegenes {")
    (println "ordering=out;")
    (dorun (map-indexed pr-ingene inputs))
    (doseq [i active
            :when (>= i n-in)]
      (let [g (nth genes i)
            in-idx (map (partial - i) (:in g))
            nm (if (:fn g) (name (:fn g))
                   (format "%.2f" (:value g)))]
        (pr-gene i nm)
        (dorun (map pr-link in-idx (repeat i)
                    (range (count in-idx))))))
    ;(println "out [label=\"output(s)\",shape=plaintext];")
    (println "node [shape=plaintext];")
    (dorun (map-indexed (fn [j i]
                          (-> (format "g%d -> out%d [style=dashed];" i j)
                              println))
                        out-idx))
    (println "}")))

(defn viz-active-genes
  "Generates an SVG graphic of the active genes graph and opens it.
   Executes the `dot` program, part of Graphviz."
  [gm]
  (let [s (with-out-str (print-active-genes gm))
        tmpf "/tmp/gpai.active-genes.svg"
        tmpdot (str tmpf ".dot")]
    (spit tmpdot s)
    (sh/sh "dot" "-Tsvg" "-o" tmpf tmpdot)
    (br/browse-url (str "file://" tmpf))))

(defn mutate-gene
  [g i]
  ;; TODO separately change one of the input links, or the function.
  ;(let [[f n] (rand-nth (seq *funcmap*))])
  (rand-gene i))

(defn mutate
  "Mutates each gene and output index with probability
   *gene-mut-rate*."
  [{:keys [genes out-idx inputs] :as gm}]
  (let [n-in (count inputs)
        gs (loop [i n-in
                  gs genes]
             (if (< i (count genes))
               (if (< (rand) *gene-mut-rate*)
                 (let [ng (mutate-gene (nth gs i) i)]
                   (recur (inc i) (assoc gs i ng)))
                 ;; no change
                 (recur (inc i) gs))
               ;; done
               gs))
        oi (mapv (fn [i]
                   (if (< (rand) *gene-mut-rate*)
                     (rand-nth (range n-in (count genes)))
                     i))
                 out-idx)]
    (assoc gm :genes gs :out-idx oi)))
