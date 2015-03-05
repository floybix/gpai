(ns org.nfrac.gpai.cgp
  "General graph-structured programs.
   i.e. Cartesian Genetic Programming with 1 row and no n-back limit.
   
   A genotype is a map of the form
   `{:nodes [], :inputs [], :out-ids [], :lang [], :options {}}`.

   The lengths of the `:inputs` and `:out-ids` vectors define the
   number of inputs and outputs, respectively. The `:inputs` vector
   contains the names of inputs (used for display only). The
   `:out-ids` vector contains the node indices to use as outputs.

   The `:lang` vector contains the available functions and constants.
   Each element must itself be a vector with:

   * functions/macros in the form `[symbol arity]`.
   * constants in the form `[value]`.

   Each node is a map like
   `{:fn 'foo, :in [1 4 1 ...]}`

   where :fn gives the node function as a namespaced symbol; the :in
   vector gives pointers to fn arguments as node indices. The number
   of arguments must match the arity of the node function.

   Some nodes may represent constants. These nodes have :fn nil, :in
   empty and instead store a `:value`.

   The leading nodes are for inputs and have :in nil and :fn nil.

   The `:options` map can hold parameters passed on to generation and
   mutation functions:

   * `:gene-mut-rate` point mutation probability for each
     gene (including function gene and input genes at each node) and
     output index (default 0.03).
   * `:erc-prob` point probability of generating an Ephemeral Random
     Constant (ERC) as opposed to a language element (default 0.0).
   * `:erc-gen` a function of no arguments to generate an ERC (default
     `#(* (gen/double) 10.0)`).

   Genotypes are compiled to functions, via `function` in this
   namespace. The function is compiled on demand and cached in genome
   metadata. Any alterations to the genome should clear/refresh the
   cache with `recache` (as functions in this namespace do). This can
   check whether the active part of the genome has in fact changed,
   since mutations often do not affect the active nodes.

   Functions can be compiled to use primitive data types by giving
   option :data-type as a symbol 'long or 'double. This will only be
   of benefit if the lang functions are declared to take and return
   the same primitive argument types."
  (:require [clojure.data.generators :as gen]))

(declare recache)

(defn validate-lang!
  [lang]
  (assert (sequential? lang))
  (assert (seq lang))
  (doseq [item lang]
    (assert (vector? item))
    (when (second item)
      (assert (symbol? (first item)))
      (assert (integer? (second item)))))
  true)

(defn genome
  [inputs nodes out-ids lang options]
  (validate-lang! lang)
  (let [gm {:inputs (vec inputs)
            :nodes (vec nodes)
            :out-ids (vec out-ids)
            :lang (vec lang)
            :options options}]
    (recache gm)))

(defn rand-link
  "Returns an index of a node before the given offset."
  [offset]
  (gen/uniform 0 offset))

(defn rand-node
  "Returns a new node at the given offset (which constrains the
   distance back of input links). Functions are chosen from lang, or
   ERCs are generated according to `:erc-prob` by calling `:erc-gen`."
  [offset lang {:as options
                :keys [erc-prob erc-gen]
                :or {erc-prob 0.0
                     erc-gen #(* (gen/double) 10.0)}}]
  (if (< (gen/double) erc-prob)
    (let [v (erc-gen)]
      {:fn nil :in [] :value v})
    (loop [tries 0]
      (when (= tries 10) (throw (Exception. "No functions for zero args.")))
      (let [[x n] (gen/rand-nth (seq lang))]
        (if-not n
          {:fn nil :in [] :value x}
          (if (and (pos? n) (zero? offset))
            (recur (inc tries))
            {:fn x :in (vec (repeatedly n #(rand-link offset)))}))))))

(defn rand-genome
  "Generates a new genome with `size` number of nodes, including input
   nodes, and with `n-out` outputs chosen randomly. Options are passed
   to `rand-node`."
  [inputs size n-out lang options]
  (let [n-in (count inputs)
        in-nodes (repeat n-in {})
        fn-nodes (map #(rand-node % lang options) (range n-in size))
        out-ids (repeatedly n-out #(gen/rand-nth (range n-in size)))]
    (genome inputs (concat in-nodes fn-nodes) out-ids lang options)))

(defn active-ids
  "Returns the set of indices corresponding to active nodes, i.e.
   those that the current output genes depend on."
  [{:keys [nodes out-ids inputs]}]
  (loop [act (set out-ids)
         more (set out-ids)]
    (if-let [i (first more)]
      (if (>= i (count inputs))
        ;; function node
        (let [nd (get nodes i)
              in-ids (:in nd)]
          (recur (into act in-ids)
                 (into (disj more i) in-ids)))
        ;; input node
        (recur act (disj more i)))
      ;; done
      act)))

(defn genome->expr
  "Converts a genome into a quoted function expression.
   This is like a macro, but at runtime."
  [{:as gm :keys [nodes out-ids inputs options]}]
  (let [data-type (:data-type options nil)
        size (count nodes)
        n-in (count inputs)
        active (sort (seq (active-ids gm)))
        ndsym (fn [id] (symbol (str "nd-" id "_")))
        args (mapv ndsym (range n-in))
        ;; can do primitive declarations on up to 4 args (clojure limit)
        prim-args? (and data-type (<= (count args) 4))
        args (if prim-args?
               (mapv #(vary-meta % assoc :tag data-type) args)
               args)
        ;; otherwise can do hints/casts on inputs: (long x) or (double x)
        init-lets (if (and data-type (not prim-args?))
                    (mapcat #(list % (list data-type %)) args)
                    [])
        lets (loop [lets (vec init-lets)
                    more (drop-while #(< % n-in) active)]
               (if-let [i (first more)]
                 (let [nd (get nodes i)
                       form (if-let [f (:fn nd)]
                              (list* f (map ndsym (:in nd)))
                              (:value nd))]
                   (recur (into lets [(ndsym i) form])
                          (next more)))
                 ;; done
                 lets))
        outs (mapv ndsym out-ids)]
    `(fn ~args (let ~lets ~outs))))

(defn- unchecked-eval
  "Eval with unchecked math to ignore integer overflow (only works for
   primitive longs)."
  [expr]
  (binding [*unchecked-math* true] (eval expr)))

(defn recache
  "Checks if a cached function is invalid, and if necessary, sets up a
   delayed evaluation to compile a new one. The genome's evaluation
   expression (using active nodes) is compared to the cached one to
   see whether recompilation is necessary. Option `:force-recache`
   overrides the check."
  [{:as gm :keys [options]}]
  (let [expr (genome->expr gm)]
    (if (and (not (:force-recache options))
             (::function (meta gm))
             (= expr (::expr (meta gm))))
      gm
      ;; else - need to recompile
      (vary-meta gm assoc
                 ::function (delay (unchecked-eval expr))
                 ::expr expr))))

(defn function
  "Returns the (cached) function corresponding to the genome."
  [gm]
  (if-let [cached-f (::function (meta gm))]
    (force cached-f)
    (recur (recache gm))))

(defn- mutate-function-gene
  "Chooses a new function for node `nd` at offset `i`. Reuses existing
   input link sequence, clipped or extended as needed."
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
  "Mutates each 'gene' (including function gene and input genes at each
   node) and output index with probability :gene-mut-rate (an option
   key), defaulting to 0.03. Other options are passed to `rand-node`."
  [{:as gm :keys [nodes out-ids inputs lang options]}]
  (let [gene-mut-rate (or (:gene-mut-rate options) 0.03)
        n-in (count inputs)
        nds (loop [i n-in
                   nds nodes]
              (if (< i (count nodes))
                (if (< (gen/double) gene-mut-rate)
                  ;; mutate function
                  (let [nnd (mutate-function-gene (get nds i) i lang options)]
                    (recur (inc i) (assoc nds i nnd)))
                  ;; otherwise, possibly mutate input links
                  (let [in (get-in nds [i :in])
                        m?s (repeatedly (count in) #(< (gen/double) gene-mut-rate))]
                    (if (some true? m?s)
                      (let [newin (mapv (fn [m? x] (if m? (rand-link i) x))
                                        m?s in)]
                        (recur (inc i) (assoc-in nds [i :in] newin)))
                      ;; no mutations to this node
                      (recur (inc i) nds))))
                ;; done
                nds))
        oi (mapv (fn [i]
                   (if (< (gen/double) gene-mut-rate)
                     (gen/rand-nth (range n-in (count nodes))) ;; exclude inputs
                     i))
                 out-ids)]
    (-> (assoc gm :nodes nds :out-ids oi)
        recache)))
