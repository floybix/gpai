(ns io.evolvability.gpai.icgp
  "Immutable CGP.

   Genomes are immutable: there is no mutation, only extension, and
   changes to which of the nodes are chosen as outputs.

   A side benefit of this scheme is that it makes a static type system
   easier to implement, since we only have to type-check new nodes,
   not the mutation of existing ones. A static type system enables the
   use of multiple data types.

   A genotype is a map of the form
   `{:nodes [], :inputs [], out-types [], :out-idx [],
     :lang [], :options {}}`.

   The lengths of the `:inputs` and `:out-idx` vectors define the
   number of inputs and outputs, respectively. The `:inputs` vector
   contains `[name type]` forms (the names are used for display only).
   The `:out-idx` vector contains the node indices to use as outputs.
   The types specified for each output are stored in `:out-types`.

   The `:lang` vector contains the available functions and constants.
   Actually macros can be used as well as functions. Each element must
   itself be a vector with:

   * functions/macros in the form `[symbol [return-type, arg-types...]]`.
   * constants in the form `[value type]`.

   Types can be anything that works with `derive?`, i.e. classes,
   symbols or keywords.

   Each node is a map like
   `{:fn 'foo, :in [1 4 1 ...], :type 'type, :arg-types '()}`

   where :fn gives the node function as a namespaced symbol; the :in
   vector gives pointers to fn arguments as node indices. The number
   of inputs must equal the arity of the node function, and the types
   of the input nodes must derive from the declared argument types.

   Some nodes may represent constants. These nodes have :fn nil, :in
   empty and instead store a `:value`. They do have a :type.

   The leading nodes are for inputs and have :in nil and :fn nil. They
   do have a :type.

   The `:options` map can hold parameters passed on to generation
   functions:

   * `:gene-mut-rate` point mutation probability for each
     output index (default 0.15).
   * `:grow-nodes` number of nodes added by the `mutate`
     function (default 2).
   * `:erc-prob` point probability of generating an Ephemeral Random
     Constant (ERC) as opposed to a language element (default 0.0).
   * `:erc-gen` a function of no arguments to generate an ERC together
     with its type (default `#(vector (* (gen/double) 10.0) Number)`).

   A continuously growing genome obviously will suffer from bloating
   problems. This is managed by letting the least-used nodes atrophy.
   Each nodes stores the number of times it was active when compiled,
   i.e. the number of generations. Nodes unused for option
   `:atrophy-steps` are set to nil. To participate in this mechanism,
   update genomes with `tick` every generation."
  (:require [clojure.data.generators :as gen]
            [io.evolvability.gpai.cgp :as cgp]))

(declare recache)

(defn type? [x]
  (or (class? x) (symbol? x) (keyword? x)))

(defn validate-lang!
  [lang]
  (assert (sequential? lang))
  (assert (seq lang))
  (doseq [[x t :as item] lang]
    (assert (vector? item))
    (assert (= 2 (count item)))
    (if (sequential? t)
      (do (assert (symbol? x))
          (assert (every? type? t)))
      (assert (type? t))))
  true)

(defn genome
  [inputs nodes out-idx out-types lang options]
  (validate-lang! lang)
  (let [gm {:inputs (vec inputs)
            :nodes (vec nodes)
            :out-idx (vec out-idx)
            :out-types (vec out-types)
            :lang lang
            :options options}]
    (recache gm)))

(defn rand-typed-link
  "Returns the index of a node compatible with `type`;
   nil is returned if none are compatible."
  [nodes type]
  (let [ok (keep-indexed (fn [i x] (when (and x (isa? (:type x) type))
                                    i))
                         nodes)]
    (when (seq ok)
      (gen/rand-nth ok))))

(defn rand-node
  "Returns a new random node for the end of the genome. Functions are
   chosen from lang, or ERCs are generated according to `:erc-prob` by
   calling `:erc-gen`."
  [{:as gm :keys [nodes lang options]}]
  (let [{:keys [erc-prob erc-gen]
         :or {erc-prob 0.0
              erc-gen #(vector (* (gen/double) 10.0) Number)}} options]
    (if (< (gen/double) erc-prob)
      (let [[v t] (erc-gen)]
        {:fn nil :in [] :value v :type t})
      (loop [sl (gen/shuffle lang)]
        (when (empty? sl)
          (throw (Exception. "No functions work with existing node types.")))
        (let [[x t] (first sl)]
          (if-not (sequential? t)
            {:fn nil :in [] :value x :type t}
            (let [[rt & ts] t
                  links (map (partial rand-typed-link nodes) ts)]
              (if (some nil? links)
                (recur (next sl))
                {:fn x
                 :in (vec links)
                 :type rt
                 :arg-types ts}))))))))

(defn add-rand-node
  "Generates a new random node and appends it to the genome.
   This does not alter the outputs because out-idx is unchanged."
  [gm]
  (update-in gm [:nodes] conj (rand-node gm)))

(defn rand-genome
  "Generates a new random genome with an initial `size` number of
   nodes, and with outputs of the number and types given."
  [inputs size out-types lang options]
  (validate-lang! lang)
  (let [in-types (map second inputs)
        in-nodes (map (partial hash-map :type) in-types)
        gm-0 {:lang lang :nodes (vec in-nodes) :options options}
        n-fn (- size (count inputs))
        nodes (:nodes (take n-fn (iterate add-rand-node gm-0)))
        out-idx (map (partial rand-typed-link nodes) out-types)]
    (genome inputs nodes out-idx out-types lang options)))

(defn active-idx
  "Returns the set of indices corresponding to active nodes, i.e.
   those that the current outputs depend on."
  [{:keys [nodes out-idx inputs]}]
  (loop [act (set out-idx)
         more (set out-idx)]
    (if-let [i (first more)]
      (if (>= i (count inputs))
        ;; function node
        (let [nd (nth nodes i)
              in-idx (:in nd)]
          (recur (into act in-idx)
                 (into (disj more i) in-idx)))
        ;; input node
        (recur act (disj more i)))
      ;; done
      act)))

(defn node-expr
  "Returns the expression for node at idx."
  [idx {:as gm :keys [nodes]}]
  (let [act (sort (seq (active-idx (assoc gm :out-idx [idx]))))
        xnds (loop [nds nodes
                    more act]
               (if-let [i (first more)]
                 (let [nd (nth nodes i)
                       ex (if-let [f (:fn nd)]
                            (list* f (map #(:expr (nth nds %))
                                          (:in nd)))
                            (:value nd))]
                   (recur (assoc-in nds [i :expr] ex) (rest more)))
                 nds))]
    (:expr (nth xnds idx))))

(defn genome->expr
  "Converts a genome into a quoted function expression.
   This is like a macro, but at runtime."
  [{:as gm :keys [nodes out-idx inputs options]}]
  (let [;data-type (:data-type options nil)
        size (count nodes)
        n-in (count inputs)
        in-types (map second inputs)
        active (sort (seq (active-idx gm)))
        syms (mapv #(symbol (str "nd-" % "_")) (range size))
        args (subvec syms 0 n-in)
        ;; do primitive casts on inputs: (long x) or (double x)
        init-lets (mapcat (fn [s t]
                            (let [x (case t
                                      Double (list 'double s)
                                      Long (list 'long s)
                                      s)]
                              (list s x)))
                          args in-types)
        lets (loop [lets (vec init-lets)
                    more (drop-while #(< % n-in) active)]
               (if-let [i (first more)]
                 (let [nd (nth nodes i)
                       form (if-let [f (:fn nd)]
                              (list* f (map (partial nth syms) (:in nd)))
                              (:value nd))]
                   (recur (into lets [(nth syms i) form])
                          (next more)))
                 ;; done
                 lets))
        outs (mapv (partial nth syms) out-idx)]
    `(fn ~args (let ~lets ~outs))))

(defn- unchecked-eval
  "Eval with unchecked math to ignore integer overflow (only works for
   primitive longs)."
  [expr]
  (binding [*unchecked-math* true] (eval expr)))

(defn function
  "Returns a function corresponding to the genome, possibly cached."
  [gm]
  (if-let [cached-f (::function (meta gm))]
    (force cached-f)
    (unchecked-eval (genome->expr gm))))

(defn recache
  "Checks if a cached function is invalid, and if necessary, sets up a
   delayed evaluation to compile a new one. The set of output nodes is
   compared to the cached one to see whether recompilation is
   necessary."
  [{:as gm :keys [out-idx]}]
  (let [curr-out (::cached-out-idx (meta gm))]
    (if (= out-idx curr-out)
      gm
      ;; else - expression changed, recompile
      (vary-meta gm assoc
                 ::function (delay (unchecked-eval (genome->expr gm)))
                 ::cached-out-idx out-idx))))

(defn- multi-assoc
  "assoc val onto all keys."
  [map keys val]
  (apply assoc map (interleave keys (repeat val))))

(defn- atrophy
  "Nullify any nodes that have not been used within a time span of
   option `:atrophy-steps` time steps (default 100)."
  [gm]
  (let [crit (:atrophy-steps (:options gm) 100)
        t (::timestep (meta gm))
        t-crit (- t crit)]
    (if (neg? t-crit)
      gm
      (let [nlu (::node-last-use (meta gm))
            die (keep (fn [[i tt]] (when (< tt t-crit) i))
                      nlu)]
        (if (empty? die)
          gm
          (-> gm
              (update-in [:nodes] multi-assoc die nil)
              (vary-meta assoc ::node-last-use (apply dissoc nlu die))))))))

(defn tick
  "Update the activity counters for active nodes and the age counter
   of the genome. Also calls atrophy."
  [gm]
  (let [act (active-idx gm)
        t-1 (::timestep (meta gm))
        t (inc (or t-1 0))
        m (-> (meta gm)
              (assoc ::timestep t)
              (update-in [::node-last-use] merge (zipmap act (repeat t)))
              (update-in [::node-use] merge-with + (zipmap act (repeat 1))))]
    (atrophy (with-meta gm m))))

(defn mutate-out-idx
  "Choose one of the outputs and point it to a randomly selected node
   of a compatible type."
  [{:as gm :keys [nodes out-idx out-types options]}]
  (let [j (gen/rand-nth (range (count out-idx)))
        type (nth out-types j)
        new-o (rand-typed-link nodes type)]
    (-> gm
        (update-in [:out-idx] assoc j new-o)
        (recache))))
