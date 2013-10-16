(ns io.evolvability.gpai.icgp
  "Immutable CGP.

   Nodes in the genome are immutable, having globally unique ids. The
   genome is mutated by inserting a modified node and removing the
   original node -- and bumping up the ids of all other nodes affected
   by it.

   A benefit of this scheme is that multiple genomes can be merged
   together, as a kind of crossover operator, by a simple union of
   their nodes.

   This implementation has a type system. All functions, inputs and
   data values are defined with a type. Functions also define the
   types of their arguments. Types can be anything that works with
   `isa?`, i.e. classes, symbols or keywords.

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

   Nodes are stored in a sorted map keyed by globally unique ids. A
   node may take inputs only from ids lower than its own. Each node is
   itself a map like
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
     with its type (default `#(vector (* (gen/double) 10.0) Double)`)."
  (:require [io.evolvability.gpai.utils :as utils]
            [clojure.data.generators :as gen]
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

(defn new-node-id
  []
  (swap! node-id-counter inc'))

(defn node
  [map last-use]
  (let [id (new-node-id)]
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
  (assoc-in gm [:nodes id] node))

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
  "Returns the key of a node compatible with `type`; nil is returned
   if none are compatible."
  [nodes type]
  (let [ok (keep (fn [[k nd]]
                   (when (isa? (:type nd) type) k))
                 nodes)]
    (when (seq ok)
      (gen/rand-nth ok))))

(defn rand-node
  "Returns a new random node for the genome. Functions are chosen from
   lang, or ERCs are generated according to `:erc-prob` by calling
   `:erc-gen`."
  [{:as gm :keys [nodes lang options]}]
  (let [t (::timestep (meta gm))
        {:keys [erc-prob erc-gen]
         :or {erc-prob 0.0
              erc-gen #(vector (* (gen/double) 10.0) Double)}} options]
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
                   :in (vec links)
                   :type rty
                   :arg-types (vec tys)} t)))))))

(defn add-rand-node
  [gm]
  (add-node gm (rand-node gm)))

(defn add-rand-nodes
  [gm n]
  (loop [gm gm
         i n]
    (if (zero? i) gm
      (recur (add-rand-node gm) (dec i)))))

(defn active-ids
  "Returns the set of node keys for the active nodes, i.e. those that
   the current outputs depend on."
  [{:keys [nodes out-ids inputs]}]
  (loop [act (set out-ids)
         more (set out-ids)]
    (if-let [id (first more)]
      (let [nd (get nodes id)
            in-ids (:in nd)
            ;; only add to search list those which we didn't already know
            new (set/difference (set in-ids) act)]
        (assert (every? #(< % id) in-ids)
                (str "input from later id: " in-ids id))
        (recur (into act new)
               (into (disj more id) new)))
      ;; done
      act)))

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

(defn child-nodes
  "Returns the keys of all nodes which depend directly on node i, not including i."
  [gm id]
  (let [nodes (:nodes gm)]
    (set (keep (fn [[k nd]]
                 (when (some #{id} (:in nd)) k))
               (subseq nodes > id)))))

(defn dependent-nodes
  "Returns the keys of all nodes which depend on node i, including i."
  [gm id]
  (let [nodes (:nodes gm)]
    (loop [ds #{id}
           more (child-nodes gm id)]
      (if-let [cid (first more)]
        (recur (conj ds cid)
               (-> (into more (child-nodes gm cid))
                   (disj cid)))
        ;; done
        ds))))

(defn discard-inactive
  "Remove a randomly chosen inactive node and any other nodes which
   depend on it."
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

(defn mutate-out-id
  "Choose one of the outputs and point it to a randomly selected node
   of a compatible type."
  ([gm]
     (let [j (gen/rand-nth (range (count (:out-ids gm))))]
       (mutate-out-id gm j)))
  ([{:as gm :keys [nodes out-types]} j]
     (let [type (nth out-types j)
           oid (rand-typed-link nodes type)]
       (when (nil? oid)
         (throw (Exception. (str "Could not find a node of out type "
                                 type))))
       (-> (update-in gm [:out-ids] assoc j oid)
           (recache)))))

(defn init-out-ids
  [gm]
  (reduce mutate-out-id gm (range (count (:out-ids gm)))))

(defn rand-genome
  "Generates a new genome with the given `inputs` and `constants`,
   plus an initial `n-rand` number of random nodes, and with outputs
   of the number and types given."
  [inputs constants out-types lang n-rand options]
  (let [gm-0 (empty-genome inputs out-types lang options)]
    (-> (reduce add-node gm-0
                (map const-node constants))
        (add-rand-nodes n-rand)
        (init-out-ids)
        (recache))))

(defn links-based-on
  "Returns a vector of ids consistent with `types`, reusing the old
   vector of ids `oids` having types `otypes`. Returns nil if a link
   of a required type can not be found."
  [nodes types otypes oids]
  (let [id->ty (zipmap oids otypes)
        ty->ids (group-by id->ty oids)]
   (loop [ids []
          tys-left types
          tym ty->ids]
     (if (empty? tys-left)
       ids
       (let [ty (first tys-left)
             id (or (first (get tym ty))
                    (rand-typed-link nodes ty))]
         (if-not id
           nil ;; could not find link of type ty
           (recur (conj ids id)
                  (rest tys-left)
                  (update-in tym [ty] rest))))))))

(defn- replace-links
  "Remap links from `old-id` to `new-id` in the `:in` vectors of `ids`."
  [gm old-id new-id ids]
  (reduce (fn [gm id]
            (update-in gm [:nodes id :in] (partial replace {old-id new-id})))
          gm ids))

(defn- exchange-one-node
  "Exchanges a node with id `oid` with the new node and id. Updates
   input ids in children to point to `adopt-id` (typically same as
   `id`). This violates the invariant that links should only point to
   nodes with smaller ids, so this helper function is not to be used
   directly."
  [gm oid [id nd] adopt-id]
  (let [cids (child-nodes gm oid)]
    (-> gm
        (add-node [id nd])
        (replace-links oid adopt-id cids)
        (update-in [:nodes] dissoc oid))))

(defn- bump-nodes
  "Replaces nodes with given `ids` (assumed to be a full subtree) with
   the same nodes given new ids. This reflects changes in nodes values
   due to upstream mutation. The ordering of ids is maintained. Also,
   output ids are remapped. Example: consider a subgraph like

   a :in [_]
   b :in [a]
   c :in [a]
   d :in [b c]
  
   exchange-one-node a ea
   b :in [ea]
   c :in [ea]
   d :in [b c]
   ea :in [_]
  
   exchange-one-node b fb
   c :in [ea]
   d :in [fb c]
   ea :in [_]
   fb :in [ea]
  
   exchange-one-node c gc
   d :in [fb gc]
   ea :in [_]
   fb :in [ea]
   gc :in [ea]
  
   exchange-one-node d hd
   ea :in [_]
   fb :in [ea]
   gc :in [ea]
   hd :in [fb gc]
"
  [gm ids]
  (let [idss (sort ids)
        newids (zipmap idss (repeatedly new-node-id))
        g (reduce (fn [gm id]
                    (let [nd (get-in gm [:nodes id])
                          nid (newids id)]
                      (exchange-one-node gm id [nid nd] nid)))
                  gm idss)]
    (update-in g [:out-ids] (partial replace newids))))

(defn exchange-node
  "Exchanges an existing node with id `oid` with a new node (assigned
   a new id). Any ids `ds` downstream of the existing node are
   remapped to the new node if its type is compatible, or to a
   randomly chosen new parent otherwise. If a new type-compatible
   parent can not be found, the children are discarded and the same
   number of new random nodes are created. Also, output ids are
   remapped if necessary."
  [gm oid nd ds]
  (let [id (new-node-id)
        dsx (disj ds oid)
        oty (get-in gm [:nodes oid :type])]
    (if (isa? (:type nd) oty)
      ;; node is compatible with previous; downstream nodes can remain
      (-> gm
          (exchange-one-node oid [id nd] id)
          (update-in [:out-ids] (partial replace {oid id}))
          (bump-nodes dsx))
      ;; incompatible. attempt to move child nodes to a new parent.
      (let [oknodes (apply dissoc (:nodes gm) oid dsx)
            nparent (rand-typed-link oknodes oty)]
        (if nparent
          ;; found a replacement parent of required type
          (-> gm
              (exchange-one-node oid [id nd] nparent)
              (update-in [:out-ids] (partial replace {oid nparent}))
              (bump-nodes dsx))
          ;; otherwise - could not find a new parent, so discard all
          ;; downstream nodes and add new random ones
          (let [g (-> gm
                      (update-in [:nodes] (fn [nds] (apply dissoc nds oid dsx)))
                      (add-node [id nd])
                      (add-rand-nodes (count dsx)))
                outs (:out-ids g)]
            (reduce (fn [g [j jid]]
                      (if (ds jid) ;; if output index j was discarded
                        ;; TODO: what if no more nodes of a required type?
                        (mutate-out-id g j)
                        g))
                    g (map-indexed vector outs))))))))

(defn mutate-node
  "Mutates node `id`, choosing uniformly between mutating the function
   itself and any one of its input links. The affected subtree is
   replaced with new node ids, maintaining the immutability of nodes.
   We avoid links to downstream nodes which would create cycles."
  [gm id]
  (let [nd (get-in gm [:nodes id])
        ds (dependent-nodes gm id)
        oknodes (apply dissoc (:nodes gm) ds)
        in (:in nd)
        tys (:arg-types nd)
        ;; what to mutate - fn itself or which input
        j (gen/uniform -1 (count in))
        nnd (if (== j -1)
              ;; mutate function itself, partially preserving input links
              (let [[_ rnd] (rand-node gm)
                    ntys (:arg-types rnd)]
                (let [newin (links-based-on oknodes ntys tys in)]
                  (if-not newin
                    nil ;; link failure
                    (assoc rnd :in newin))))
              ;; otherwise - mutate input link j
              (let [jty (get-in nd [:arg-types j])
                    jid (rand-typed-link oknodes jty)]
                (if-not jid
                  nil ;; link failure
                  (assoc-in nd [:in j] jid))))]
    (if-not nnd
      gm ;; link failure; skip mutation
      (exchange-node gm id nnd ds))))

(defn mutate
  "In effect, mutates nodes of the genome as in normal CGP. Possible
   modifications are:

   * change one of the input links of a function node;
   * change a node to another of the same type (partially preserving
     input links, depending on the function arguments);
   * change a node to another of a different type, detaching any
     downstream nodes and re-attaching them if possible to another
     node of the original type;
   * change one of the output ids.

   Each node and output id is mutated with probability
   `:node-mut-rate` (an option key) defaulting to 0.03."
  [{:as gm :keys [nodes out-ids options]}]
  (let [node-mut-rate (or (:node-mut-rate options) 0.03)
        all (set (keys (:nodes gm)))
        ins (set (:in-ids gm))
        oks (set/difference all ins)
        g (reduce (fn [g j]
                    (if (< (gen/double) node-mut-rate)
                      (mutate-out-id g j)
                      g))
                  gm (range (count out-ids)))
        g2 (reduce (fn [g id]
                     (if (< (gen/double) node-mut-rate)
                       (do
                         ;(println "Mutating node" id)
                         ;(println "Old nodes:")
                         ;(prn (:nodes g))
                         (let [x (mutate-node g id)]
                           ;(println "New nodes:")
                           ;(prn (:nodes x))
                           x)
                         )
                       g))
                   g
                   ;; mutate ids in decreasing order so remaining ids are not
                   ;; affected by mutations so far
                   (sort > oks))]
    (recache g2)))

(defn vary-neutral
  "Vary the currently inactive parts of the genome by either adding or
   discarding nodes depending on the `target-size` (number of nodes)."
  [gm target-size]
  (if (> (count (:nodes gm)) target-size)
    (discard-inactive gm)
    (add-rand-node gm)))

(defn print-codesizes
  [i xs _]
  (let [szs (map (comp count active-ids) xs)
        szo (sort szs)
        sz-max (last szo)
        sz-min (first szo)
        sz-med (utils/median szo)]
    (println (format "Gen %d: codesizes [ %8d  %8d  %8d ]"
                     i sz-min sz-med sz-max))))
