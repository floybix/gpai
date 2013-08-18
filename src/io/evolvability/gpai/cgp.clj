(ns io.evolvability.gpai.cgp
  "General graph-structured programs.
   i.e. Cartesian Genetic Programming with 1 row and no n-back limit.
   
   A genotype is a map of the form
   `{:nodes [], :inputs [], :out-idx [], :lang [], :options {}}`.

   The lengths of the `:inputs` and `:out-idx` vectors define the
   number of inputs and outputs, respectively. The `:inputs` vector
   contains the names of inputs (used for display only). The
   `:out-idx` vector contains the node indices to use as outputs.

   The `:lang` vector contains the available functions and constants.
   Each element must itself be a vector, with functions given as
   [fn-symbol arity], and constants as [value nil] or just [value].
   Function arities can be looked up with `utils/arity` which also
   returns nil for non-symbols.

   Each node is a map like
   `{:fn 'foo, :in [1 4 1 ...]}`

   where :fn gives the node function as a namespaced symbol; the :in
   vector gives pointers to fn arguments as a number of cells back.
   The number of arguments must match the arity of the node function.

   Some nodes may represent constants. These nodes have :fn nil, :in
   empty and instead store a `:value`.

   The leading nodes are for inputs and have :in nil and :fn nil.

   The `:options` map can hold parameters passed on to generation and
   mutation functions:
   * :gene-mut-rate point mutation probability for each
     gene (including function gene and input genes at each node) and
     output index (default 0.03).
   * :erc-prob point probability of generating an Ephemeral Random
     Constant (ERC) as opposed to an input symbol given that we are
     generating a terminal (default 0.0).
   * :erc-gen (default #(rand 10.0)) a function of no arguments to
     generate an ERC."
  (:require [clojure.java.shell :as sh]
            [clojure.java.browse :as br]))

(defn- rand-link
  [offset]
  (inc (rand-int offset)))

(defn rand-node
  "Returns a new node at the given offset (which constrains the
   distance back of input links). ERCs are generated according to
   erc-prob by calling erc-gen. Otherwise functions are chosen from
   lang."
  [offset lang {:as options
                :keys [erc-prob erc-gen]
                :or {erc-prob 0.0
                     erc-gen #(rand 10.0)}}]
  (if (< (rand) erc-prob)
    (let [v (erc-gen)]
      {:fn nil :in [] :value v})
    (let [[x n] (rand-nth (seq lang))]
      (if n
        {:fn x :in (vec (repeatedly n #(rand-link offset)))}
        {:fn nil :in [] :value x}))))

(defn rand-genome
  "Generates a new genome with `size` number of nodes, including input
   nodes, and with `n-out` outputs chosen randomly. Options are passed
   to `rand-node`."
  [inputs size n-out lang options]
  (let [n-in (count inputs)
        in-nodes (repeat n-in {})
        fn-nodes (map #(rand-node % lang options) (range n-in size))]
    {:inputs (vec inputs)
     :nodes (vec (concat in-nodes fn-nodes))
     :out-idx (vec (repeatedly n-out #(rand-nth (range n-in size))))
     :lang lang
     :options options}))

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
  [{:as gm :keys [nodes out-idx inputs]} input-vals]
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
  [{:as gm :keys [nodes out-idx inputs]} input-vals]
  (let [nds (eval-genome gm input-vals)]
    (mapv #(get-in nds [% :value]) out-idx)))

(defn print-active-nodes
  "Prints the graph of active nodes in DOT format."
  [{:as gm :keys [nodes out-idx inputs]}]
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
  "Converts a genome into a quoted function expression.
   This is like a macro, but at runtime."
  [{:as gm :keys [nodes out-idx inputs]}]
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
  "Converts a genome into a function, using `eval`. This is preferred
   to `genome-outputs` as the function, once compiled, will be much
   faster to evaluate."
  [gm]
  (eval (genome->expr gm)))

(defn- mutate-function-gene
  [nd i lang options]
  (let [nnd (rand-node i lang options)
        k (count (:in nd))
        nk (count (:in nnd))
        in (if (<= nk k)
             ;; new args are a subset of old args
             (subvec (:in nd) 0 nk)
             ;; new args need extras beyond old args
             (into (:in nd) (subvec (:in nnd) k)))]
    (assoc nnd :in in)))

(defn mutate
  "Mutates each gene (including function gene and input genes at each
   node) and output index with probability :gene-mut-rate (an option
   key), defaulting to 0.03. Other options are passed to `rand-node`."
  [{:as gm :keys [nodes out-idx inputs lang options]}]
  (let [gene-mut-rate (or (:gene-mut-rate options) 0.03)
        n-in (count inputs)
        nds (loop [i n-in
                   nds nodes]
              (if (< i (count nodes))
                (if (< (rand) gene-mut-rate)
                  ;; mutate function
                  (let [nnd (mutate-function-gene (nth nds i) i lang options)]
                    (recur (inc i) (assoc nds i nnd)))
                  ;; otherwise, possibly mutate input links
                  (let [in (get-in nds [i :in])
                        m?s (repeatedly (count in) #(< (rand) gene-mut-rate))]
                    (if (some true? m?s)
                      (let [nin (mapv (fn [m? x] (if m? (rand-link i) x))
                                      m?s in)]
                        (recur (inc i) (assoc-in nds [i :in] nin)))
                      ;; no mutations to this node
                      (recur (inc i) nds))))
                ;; done
                nds))
        oi (mapv (fn [i]
                   (if (< (rand) gene-mut-rate)
                     (rand-nth (range n-in (count nodes))) ;; exclude inputs
                     i))
                 out-idx)]
    (assoc gm :nodes nds :out-idx oi)))
