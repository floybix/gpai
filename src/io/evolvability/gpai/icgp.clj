(ns io.evolvability.gpai.icgp
  "Immutable CGP.

   Genomes are immutable: there is no mutation, only extension, as well
   as changes to which of the nodes are chosen as outputs.

   A benefit of this scheme is that it makes a static type system
   feasible, since we only have to type-check new nodes, not the
   mutation of existing ones. A static type system enables the use of
   multiple data types.

   A genotype is a map of the form
   `{:nodes [], :inputs [], out-types [], :out-idx [],
     :lang [], :options {}}`.

   The lengths of the `:inputs` and `:out-idx` vectors define the
   number of inputs and outputs, respectively. The `:inputs` vector
   contains `[name type]` forms (the names are used for display only).
   The `:out-idx` vector contains the node indices to use as outputs.
   The types specified for each output are stored in `:out-types`.

   The `:lang` vector contains the available functions and macros.
   Each element must itself be a vector in the form
     `[symbol [return-type, arg-types...]]`.

   Types can be anything that works with `isa?`, i.e. classes, symbols
   or keywords.

   Each node is a map like
   `{:fn 'foo, :in [1 4 1 ...], :type 'type, :arg-types '()}`

   where :fn gives the node function as a namespaced symbol; the :in
   vector gives pointers to fn arguments as node indices. The number
   of inputs must equal the arity of the node function, and the types
   of the input nodes must derive from the declared argument types.

   Some nodes may represent constants. These nodes have no `:fn` or
   `:in` and instead store a `:value` and a `:type`. The leading nodes
   are for inputs and have a similar structure to constant nodes.

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
   problems. This is managed by letting the least-recently-used nodes
   atrophy. Each node stores the number of generations in which it was
   active. Nodes unused for option `:atrophy-steps` are set to nil. To
   participate in this mechanism, update genomes with `tick` every
   generation."
  (:require [clojure.data.generators :as gen]
            [io.evolvability.gpai.cgp :as cgp]))

(def default-atrophy-steps 200)
(def const-atrophy-steps Long/MAX_VALUE)

(defn type? [x]
  (or (class? x) (symbol? x) (keyword? x)))

(defn validate-lang!
  [lang]
  (assert (sequential? lang))
  (assert (seq lang))
  (doseq [[x ty :as item] lang]
    (assert (vector? item))
    (assert (= 2 (count item)))
    (if (sequential? ty)
      (do (assert (symbol? x))
          (assert (every? type? ty)))
      (assert (type? ty))))
  true)

(defn node
  [map last-use]
  (with-meta map
    {::last-use last-use}))

(defn const-node
  "Returns a new constant node."
  [[value type]]
  (node {:value value
         :type type} const-atrophy-steps))

(defn empty-genome
  "Returns a genome consisting only of input nodes. It is not usable
   initially because the `:out-idx` values are nil. After more nodes
   are added, use `init-out-idx` to set them."
  [inputs out-types lang options]
  (validate-lang! lang)
  (let [in-nodes (map (fn [[nm typ]]
                        (const-node [(symbol nm) typ]))
                      inputs)]
    (with-meta
      {:inputs (vec inputs)
       :nodes (vec in-nodes)
       :out-idx (vec (repeat (count out-types) nil))
       :out-types (vec out-types)
       :lang (vec lang)
       :options options}
      {::timestep 0})))

(defn rand-typed-link
  "Returns the index of a node compatible with `type`; nil is returned
   if none are compatible. Argument `min-lu` limits linked nodes by a
   minimum last-use time, which defines the minimum lifetime before
   the new node can atrophy."
  ([nodes type]
     (rand-typed-link nodes 0 type))
  ([nodes min-lu type]
     (let [ok (keep-indexed (fn [i x]
                              (when (and x (isa? (:type x) type)
                                         (>= (::last-use (meta x)) min-lu))
                                i))
                            nodes)]
       (when (seq ok)
         (gen/rand-nth ok)))))

(defn rand-node
  "Returns a new random node for the end of the genome. Functions are
   chosen from lang, or ERCs are generated according to `:erc-prob` by
   calling `:erc-gen`."
  [{:as gm :keys [nodes lang options]}]
  (let [t (::timestep (meta gm) 0)
        {:keys [erc-prob erc-gen atrophy-steps]
         :or {atrophy-steps default-atrophy-steps
              erc-prob 0.0
              erc-gen #(vector (* (gen/double) 10.0) Number)}} options
        min-lu (- t (quot atrophy-steps 2))]
    (if (< (gen/double) erc-prob)
      (let [[v ty] (erc-gen)]
        (node {:value v :type ty} t))
      (loop [sl (gen/shuffle lang)]
        (when (empty? sl)
          (throw (Exception. "No functions work with existing node types.")))
        (let [[x [rty & tys]] (first sl)
              links (map (partial rand-typed-link nodes min-lu) tys)]
          (if (some nil? links)
            (recur (next sl))
            ;; new node must atrophy when any dependencies do (or before)
            (let [lu (if (seq links)
                       (apply min t (map (comp ::last-use meta #(nth nodes %))
                                         links))
                       t)]
              (node {:fn x
                     :in (vec links)
                     :type rty
                     :arg-types tys} lu))))))))

(defn add-node
  "Appends a node the genome. The node should have been generated with
   reference to the existing nodes. This does not alter the genome
   output because out-idx is unchanged."
  [gm node]
  (update-in gm [:nodes] conj node))

(defn add-rand-node
  [gm]
  (add-node gm (rand-node gm)))

(defn active-idx
  "Returns the set of indices corresponding to active nodes, i.e.
   those that the current outputs depend on."
  [{:as gm :keys [nodes out-idx]}]
  (loop [act (set out-idx)
         more (set out-idx)]
    (if-let [i (first more)]
      (let [nd (nth nodes i)
            in (:in nd)]
        (if in
          (recur (into act in)
                 (into (disj more i) in))
          (recur act (disj more i))))
      ;; done
      act)))

(defn node-expr
  "Returns the expression for node at idx."
  [gm idx]
  (let [act (sort (seq (active-idx (assoc gm :out-idx [idx]))))
        xnds (loop [nds (:nodes gm)
                    more act]
               (if-let [i (first more)]
                 (let [nd (nth nds i)
                       ex (if-let [f (:fn nd)]
                            (list* f (map #(:expr (nth nds %))
                                          (:in nd)))
                            (:value nd))]
                   (recur (assoc-in nds [i :expr] ex) (rest more)))
                 nds))]
    (:expr (nth xnds idx))))

(defn out-exprs
  "Returns a sequence of expressions for the output nodes."
  [gm]
  (map (partial node-expr gm) (:out-idx gm)))

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
        init-lets (mapcat (fn [s ty]
                            (let [x (case ty
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

(defn recache
  "Checks if a cached function is invalid, and if necessary, sets up a
   delayed evaluation to compile a new one. The set of output nodes is
   compared to the cached one to see whether recompilation is
   necessary. Option `:force-recache` overrides the check."
  [{:as gm :keys [out-idx options]}]
  (if (and (not (:force-recache options))
           (::function (meta gm))
           (= out-idx (::cached-out-idx (meta gm))))
    gm
    ;; else - need to recompile
    (vary-meta gm assoc
               ::function (delay (unchecked-eval (genome->expr gm)))
               ::cached-out-idx out-idx)))

(defn function
  "Returns the (cached) function corresponding to the genome."
  [gm]
  (if-let [cached-f (::function (meta gm))]
    (force cached-f)
    (recur (recache gm))))

(defn atrophy-lru
  "Remove the least recently used node."
  [gm]
  (let [t (::timestep (meta gm))
        nodes (:nodes gm)
        lu (fn [i] (::last-use (meta (nth nodes i))
                              Long/MAX_VALUE))
        min-i (apply min-key lu (range (count nodes)))]
    (if (< (lu min-i) t)
      (update-in gm [:nodes] assoc min-i nil)
      gm)))

(defn- atrophy
  "Nullify any nodes that have not been used within a time span of
   option `:atrophy-steps` time steps."
  [gm]
  (let [crit (:atrophy-steps (:options gm) default-atrophy-steps)
        t (::timestep (meta gm))
        t-crit (- t crit)]
    (if (neg? t-crit)
      gm
      (let [nodes (mapv (fn [nd]
                          (when (and nd (>= (::last-use (meta nd)) t-crit)) nd))
                        (:nodes gm))]
        (assoc gm :nodes nodes)))))

(defn tick
  "Update the activity counters for active nodes and the age counter
   of the genome."
  [gm]
  (let [act (active-idx gm)
        t-1 (::timestep (meta gm))
        t (inc' t-1)
        nodes (reduce (fn [nds i]
                        (update-in nds [i] vary-meta
                                   update-in [::last-use] max t))
                      (:nodes gm) act)]
    (-> (assoc gm :nodes nodes)
        (vary-meta assoc ::timestep t))))

(defn mutate-out-idx
  "Choose one of the outputs and point it to a randomly selected node
   of a compatible type."
  ([gm]
     (let [j (gen/rand-nth (range (count (:out-idx gm))))]
       (mutate-out-idx gm j)))
  ([{:as gm :keys [nodes out-types]} j]
     (let [type (nth out-types j)
           new-o (rand-typed-link nodes type)]
       (when (nil? new-o)
         (throw (Exception. (str "Could not find a node of out type "
                                 type))))
       (-> (update-in gm [:out-idx] assoc j new-o)
           (recache)))))

(defn init-out-idx
  [gm]
  (reduce mutate-out-idx gm (range (count (:out-idx gm)))))

(defn rand-genome
  "Generates a new genome with the given `inputs` and `constants`,
   plus an initial `n-rand` number of random nodes, and with outputs
   of the number and types given."
  [inputs constants out-types lang n-rand options]
  (let [gm-0 (empty-genome inputs out-types lang options)
        gm-1 (reduce add-node gm-0
                     (map const-node constants))]
    (-> (iterate add-rand-node gm-1)
        (nth n-rand)
        (init-out-idx)
        (recache))))
