(ns gpai.cgp
  "General graph-structured programs.
   i.e. CGP with 1 row and no n-back limit.
   
   A genotype is a map of the form
   `{:nodes [], :inputs [], :out-idx []}`.

   The inputs vector defines the number of inputs and contains their
   names (used for display only). The `:out-idx` vector defines the
   number of outputs and contains the node indices to use as outputs.

   Each node is a map of the form
   `{:fn 'foo, :in [1 4 1 ...]}`

   where :fn gives the function as a (namespaced) symbol; the :in
   vector gives pointers to fn arguments as numbers of cells back. The
   number of arguments must match the arity of the function. Some
   nodes may represent Ephemeral Random Constants. These nodes
   have :fn nil, :in empty and store a :value.

   The leading nodes are for inputs and have :in nil and :fn nil."
  (:require [clojure.java.shell :as sh]
            [clojure.java.browse :as br]))

(def ^{:doc "Map of functions (as namespaced symbols) to their arity."}
  ^:dynamic *funcmap*)
(def ^:dynamic *erc-probability* 0.2)
(def ^:dynamic *erc-generator* #(rand 10.0))
(def ^:dynamic *gene-mut-rate* 0.03)

(defn rand-link
  [offset]
  (inc (rand-int offset)))

(defn rand-node
  "Returns a new node at the given offset (which constrains the
   distance back of input links). ERCs are generated according to
   *erc-probability* by calling *erc-generator*. Otherwise functions
   are chosen from *funcmap*."
  [offset]
  (if (< (rand) *erc-probability*)
    (let [v (*erc-generator*)]
      {:fn nil :in [] :value v})
    (let [[f n] (rand-nth (seq *funcmap*))]
      {:fn f :in (vec (repeatedly n #(rand-link offset)))})))

(defn rand-genome
  [inputs size n-out]
  (let [n-in (count inputs)
        in-nodes (repeat n-in {})
        fn-nodes (map rand-node (range n-in size))]
    {:inputs (vec inputs)
     :nodes (vec (concat in-nodes fn-nodes))
     :out-idx (vec (repeatedly n-out #(rand-nth (range n-in size))))}))

(defn active-idx
  "Returns the set of indices corresponding to active nodes, i.e.
   those that the current output genes depend on."
  [{:keys [nodes out-idx inputs]}]
  (loop [act (set out-idx)
         more (set out-idx)]
    (if-let [i (first more)]
      (if (>= i (count inputs))
        ;; function node
        (let [nd (nth nodes i)
              in-idx (map (partial - i) (:in nd))]
          (recur (into act in-idx)
                 (into (disj more i) in-idx)))
        ;; input node
        (recur act (disj more i)))
      ;; done
      act)))

(defn eval-genome
  "Takes a vector of input values. Associates values with all active
  nodes by calling the respective functions, storing the result in
  :value. Returns the vector of nodes."
  [{:keys [nodes out-idx inputs] :as gm} input-vals]
  (let [n-in (count inputs)
        active (sort (seq (active-idx gm)))
        in-nodes (map #(hash-map :value %) input-vals)
        nds (apply assoc nodes (interleave (range n-in)
                                          in-nodes))]
    (loop [nds nds
           to-eval (drop-while #(< % n-in) active)]
      (if-let [i (first to-eval)]
        (let [nd (nth nds i)
              in-idx (map (partial - i) (:in nd))
              invals (map #(get-in nds [% :value]) in-idx)
              v (if-let [f (:fn nd)]
                  (apply (resolve f) invals)
                  (:value nd))]
          (recur (assoc-in nds [i :value] v)
                 (next to-eval)))
        ;; done
        nds))))

(defn genome-outputs
  "Evaluates and returns the genome outputs using given input values."
  [{:keys [nodes out-idx inputs] :as gm} input-vals]
  (let [nds (eval-genome gm input-vals)]
    (mapv #(get-in nds [% :value]) out-idx)))

(defn print-active-nodes
  "Prints the graph of active nodes in DOT format."
  [{:keys [nodes out-idx inputs] :as gm}]
  (let [n-in (count inputs)
        active (sort (seq (active-idx gm)))
        pr-node (fn [i nm]
                  (println (format "nd%d [label=\"%s\"];"
                                   i (str nm))))
        pr-in-node (fn [i nm]
                     (println (format "nd%d [label=\"%s\",shape=box];"
                                      i (str nm))))
        pr-link (fn [i1 i2 j]
                  (println (format "nd%d -> nd%d [label=%d];"
                                   i1 i2 j)))]
    (println "digraph activenodes {")
    (println "ordering=out;")
    (dorun (map-indexed pr-in-node inputs))
    (doseq [i active
            :when (>= i n-in)]
      (let [nd (nth nodes i)
            in-idx (map (partial - i) (:in nd))
            nm (if (:fn nd) (name (:fn nd))
                   (format "%.2f" (:value nd)))]
        (pr-node i nm)
        (dorun (map pr-link in-idx (repeat i)
                    (range (count in-idx))))))
    ;(println "out [label=\"output(s)\",shape=plaintext];")
    (println "node [shape=plaintext];")
    (dorun (map-indexed (fn [j i]
                          (-> (format "nd%d -> out%d [style=dashed];" i j)
                              println))
                        out-idx))
    (println "}")))

(defn viz-active-nodes
  "Generates an SVG graphic of the active nodes graph and opens it.
   Executes the `dot` program, part of Graphviz."
  [gm]
  (let [s (with-out-str (print-active-nodes gm))
        tmpf "/tmp/gpai.active-nodes.svg"
        tmpdot (str tmpf ".dot")]
    (spit tmpdot s)
    (sh/sh "dot" "-Tsvg" "-o" tmpf tmpdot)
    (br/browse-url (str "file://" tmpf))))

(defn genome->expr
  [{:keys [nodes out-idx inputs] :as gm}]
  (let [size (count nodes)
        n-in (count inputs)
        active (sort (seq (active-idx gm)))
        syms (mapv #(symbol (str "nd-" % "_")) (range size))
        lets (loop [lets []
                    more (drop-while #(< % n-in) active)]
               (if-let [i (first more)]
                 (let [nd (nth nodes i)
                       in-idx (map (partial - i) (:in nd))
                       form (if-let [f (:fn nd)]
                              (list* f (map (partial nth syms) in-idx))
                              (:value nd))]
                   (recur (into lets [(nth syms i) form])
                          (next more)))
                 ;; done
                 lets))
        args (subvec syms 0 n-in)
        outs (mapv (partial nth syms) out-idx)]
    `(fn ~args (let ~lets ~outs))))

(defn genome->fn
  [gm]
  (eval (genome->expr gm)))

(defn mutate-function-gene
  [nd i]
  (let [nnd (rand-node i)
        k (count (:in nd))
        nk (count (:in nnd))
        in (if (<= nk k)
             ;; new args are a subset of old args
             (subvec (:in nd) 0 nk)
             ;; new args need extras beyond old args
             (into (:in nd) (subvec (:in nnd) k)))]
    (assoc nnd :in in)))

(defn mutate
  "Mutates each gene and output index with probability
   *gene-mut-rate*."
  [{:keys [nodes out-idx inputs] :as gm}]
  (let [n-in (count inputs)
        nds (loop [i n-in
                   nds nodes]
              (if (< i (count nodes))
                (if (< (rand) *gene-mut-rate*)
                  ;; mutate function
                  (let [nnd (mutate-function-gene (nth nds i) i)]
                    (recur (inc i) (assoc nds i nnd)))
                  ;; otherwise, possibly mutate input links
                  (let [in (get-in nds [i :in])
                        m?s (repeatedly (count in) #(< (rand) *gene-mut-rate*))]
                    (if (some true? m?s)
                      (let [nin (mapv (fn [m? x] (if m? (rand-link i) x))
                                      m?s in)]
                        (recur (inc i) (assoc-in nds [i :in] nin)))
                      ;; no mutations to this node
                      (recur (inc i) nds))))
                ;; done
                nds))
        oi (mapv (fn [i]
                   (if (< (rand) *gene-mut-rate*)
                     (rand-nth (range n-in (count nodes))) ;; exclude inputs
                     i))
                 out-idx)]
    (assoc gm :nodes nds :out-idx oi)))
