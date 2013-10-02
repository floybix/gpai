(ns io.evolvability.gpai.icgp
  "Immutable CGP.

   Genomes are immutable: there is no mutation, only extension, as well
   as changes to which of the nodes are chosen as outputs.

   A benefit of this scheme is that it makes a static type system
   feasible, since we only have to type-check new nodes, not the
   mutation of existing ones. A static type system enables the use of
   multiple data types.

   A genotype is a map of the form
   `{:nodes {}, :inputs [], out-types [], :out-ids [],
     :lang [], :options {}}`.

   The lengths of the `:inputs` and `:out-ids` vectors define the
   number of inputs and outputs, respectively. The `:inputs` vector
   contains `[name type]` forms (the names are used for display only).
   The `:out-ids` vector contains the node keys to use as outputs.
   The types specified for each output are stored in `:out-types`.

   The `:lang` vector contains the available functions and macros.
   Each element must itself be a vector in the form
     `[symbol [return-type, arg-types...]]`.

   Types can be anything that works with `isa?`, i.e. classes, symbols
   or keywords.

   Nodes are stored in a map keyed by globally unique ids. Each node
   is itself a map like
     `{:fn 'foo, :in [1 4 1 ...], :type 'type}`
   where

   * `:fn` gives the node function as a namespaced symbol. For
     constant nodes this is nil and instead a `:value` is stored.
   * the `:in` vector gives pointers to fn arguments as node keys. The
     number of inputs must equal the arity of the node function, and
     the types of the input nodes must derive from the argument types
     declared in the genome language (kept in a node as `:arg-types`).
   * `:type` gives the type of the node value.

   The leading nodes are for inputs and have `:fn` nil.

   The `:options` map can hold parameters passed on to generation
   functions:

   * `:erc-prob` point probability of generating an Ephemeral Random
     Constant (ERC) as opposed to a language element (default 0.0).
   * `:erc-gen` a function of no arguments to generate an ERC together
     with its type (default `#(vector (* (gen/double) 10.0) Number)`).

   A continuously growing genome obviously will suffer from bloating
   problems. This is managed by discarding unused nodes. To
   participate in this mechanism, update genomes with `tick` every
   generation."
  (:require [clojure.data.generators :as gen]
            [clojure.set :as set]))

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

(def node-id-counter (atom 0))

(defn node
  [map last-use]
  (let [id (swap! node-id-counter inc')]
    [id (with-meta map
          {::last-use last-use})]))

(defn const-node
  "Returns a new constant or input node."
  ([[value type]]
     (const-node [value type] 0))
  ([[value type] t]
     (node {:value value
            :type type} t)))

(defn add-node
  "Adds a node to the genome with given globally unique id. The node
   may reference existing nodes. This does not alter the genome output
   because out-ids is unchanged."
  [gm [id node]]
  (let [act (-> (mapcat (fn [id]
                          (get-in gm [:nodes id :activates]))
                        (:in node))
                (set)
                (conj id))]
    (update-in gm [:nodes] assoc id
               (assoc node :activates act))))

(defn empty-genome
  "Returns a genome consisting only of input nodes. It is not usable
   initially because the `:out-ids` values are nil. After more nodes
   are added, use `init-out-ids` to set them."
  [inputs out-types lang options]
  (validate-lang! lang)
  (let [in-nodes (map (fn [[nm typ]]
                        (const-node [(symbol nm) typ] 0))
                      inputs)
        gm0 (with-meta
              {:inputs (vec inputs)
               :in-ids (mapv first in-nodes)
               :nodes (sorted-map)
               :out-ids (vec (repeat (count out-types) nil))
               :out-types (vec out-types)
               :lang (vec lang)
               :options options}
              {::timestep 0})]
    (reduce add-node gm0 in-nodes)))

(defn rand-typed-link
  "Returns the [key value] of a node compatible with `type`; nil is
   returned if none are compatible."
  [nodes type]
  (let [ok (filter (fn [[k nd]]
                     (isa? (:type nd) type))
                   nodes)]
    (when (seq ok)
      (gen/rand-nth ok))))

(defn rand-node
  "Returns a new random node for the end of the genome. Functions are
   chosen from lang, or ERCs are generated according to `:erc-prob` by
   calling `:erc-gen`."
  [{:as gm :keys [nodes lang options]}]
  (let [t (::timestep (meta gm))
        {:keys [erc-prob erc-gen]
         :or {erc-prob 0.0
              erc-gen #(vector (* (gen/double) 10.0) Number)}} options]
    (if (< (gen/double) erc-prob)
      (let [[v ty] (erc-gen)]
        (node {:value v :type ty} t))
      (loop [sl (gen/shuffle lang)]
        (when (empty? sl)
          (throw (Exception. "No functions work with existing node types.")))
        (let [[x [rty & tys]] (first sl)
              links (map (partial rand-typed-link nodes) tys)]
          (if (some nil? links)
            (recur (next sl))
            (node {:fn x
                   :in (mapv first links)
                   :type rty
                   :arg-types tys} t)))))))

(defn add-rand-node
  [gm]
  (add-node gm (rand-node gm)))

(defn active-ids
  "Returns the set of node keys for the active nodes, i.e. those that
   the current outputs depend on."
  [{:as gm :keys [nodes out-ids]}]
  (apply set/union
         (map (fn [id]
                (:activates (get nodes id)))
              out-ids)))

(defn node-expr
  "Returns the expression for node at key `id`."
  [gm id]
  (let [act (sort (seq (active-ids (assoc gm :out-ids [id]))))
        xnds (loop [nds (:nodes gm)
                    more act]
               (if-let [i (first more)]
                 (let [nd (get nds i)
                       ex (if-let [f (:fn nd)]
                            (list* f (map #(:expr (get nds %))
                                          (:in nd)))
                            (:value nd))]
                   (recur (assoc-in nds [i :expr] ex) (rest more)))
                 nds))]
    (:expr (get xnds id))))

(defn out-exprs
  "Returns a sequence of expressions for the output nodes."
  [gm]
  (map (partial node-expr gm) (:out-ids gm)))

(defn genome->expr
  "Converts a genome into a quoted function expression.
   This is like a macro, but at runtime."
  [{:as gm :keys [nodes in-ids out-ids inputs options]}]
  (let [in-types (map second inputs)
        active (active-ids gm)
        ndsym (fn [id] (symbol (str "nd-" id "_")))
        args (mapv ndsym in-ids)
        ;; do primitive casts on inputs: (long x) or (double x)
        init-lets (mapcat (fn [s ty]
                            (let [x (case ty
                                      Double (list 'double s)
                                      Long (list 'long s)
                                      s)]
                              (list s x)))
                          args in-types)
        lets (loop [lets (vec init-lets)
                    more (sort (set/difference active (set in-ids)))]
               (if-let [id (first more)]
                 (let [nd (get nodes id)
                       form (if-let [f (:fn nd)]
                              (list* f (map ndsym (:in nd)))
                              (:value nd))]
                   (recur (into lets [(ndsym id) form])
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
   delayed evaluation to compile a new one. The set of output nodes is
   compared to the cached one to see whether recompilation is
   necessary. Option `:force-recache` overrides the check."
  [{:as gm :keys [out-ids options]}]
  (if (and (not (:force-recache options))
           (::function (meta gm))
           (= out-ids (::cached-out-ids (meta gm))))
    gm
    ;; else - need to recompile
    (vary-meta gm assoc
               ::function (delay (unchecked-eval (genome->expr gm)))
               ::cached-out-ids out-ids)))

(defn function
  "Returns the (cached) function corresponding to the genome."
  [gm]
  (if-let [cached-f (::function (meta gm))]
    (force cached-f)
    (recur (recache gm))))

(defn dependent-nodes
  "Returns the keys of all nodes which depend on node i, including i."
  [gm id]
  (let [nodes (:nodes gm)]
    (set (map key (filter (fn [[k nd]]
                            (contains? (:activates nd) id))
                          (subseq nodes >= id))))))

(defn discard-node
  "Remove a randomly chosen node and any other nodes which depend on it."
  [gm]
  (let [all (set (keys (:nodes gm)))
        act (active-ids gm)
        ins (set (:in-ids gm))
        off (set/difference all act ins)
        kill (gen/rand-nth (seq off))
        kills (dependent-nodes gm kill)]
    (update-in gm [:nodes] (fn [nds] (apply dissoc nds kills)))))

(defn tick
  "Update the activity counters for active nodes and the age counter
   of the genome."
  [gm]
  (let [act (active-ids gm)
        t-1 (::timestep (meta gm))
        t (inc' t-1)
        nodes (reduce (fn [nds i]
                        (update-in nds [i] vary-meta
                                   update-in [::last-use] max t))
                      (:nodes gm) act)]
    (-> (assoc gm :nodes nodes)
        (vary-meta assoc ::timestep t))))

(defn mutate-out-ids
  "Choose one of the outputs and point it to a randomly selected node
   of a compatible type."
  ([gm]
     (let [j (gen/rand-nth (range (count (:out-ids gm))))]
       (mutate-out-ids gm j)))
  ([{:as gm :keys [nodes out-types]} j]
     (let [type (nth out-types j)
           [oid _] (rand-typed-link nodes type)]
       (when (nil? oid)
         (throw (Exception. (str "Could not find a node of out type "
                                 type))))
       (-> (update-in gm [:out-ids] assoc j oid)
           (recache)))))

(defn init-out-ids
  [gm]
  (reduce mutate-out-ids gm (range (count (:out-ids gm)))))

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
        (init-out-ids)
        (recache))))

(defn vary-neutral
  "Vary the currently inactive parts of the genome by either adding or
   discarding nodes depending on the `target-size` (number of nodes)."
  [gm target-size]
  (if (> (count (:nodes gm)) target-size)
    (discard-node gm)
    (add-rand-node gm)))
