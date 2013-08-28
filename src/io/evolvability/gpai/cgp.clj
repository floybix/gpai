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
     generate an ERC.

   For performance, a genotype is compiled to a function, which can be
   accessed with `function` in this namespace. The option
   `:precompile?` (default true) determines whether the function is
   compiled immediately on construction of the genome and cached in
   metadata. Any alterations to the genome should clear/refresh the
   cache with `recache` (as functions in this namespace do). It can
   check whether the active part of the genome has in fact changed,
   since mutations often do not affect the active nodes.

   Functions can be compiled with primitive argument declarations (or
   casts) by giving option :data-type as a symbol 'long or 'double.
   This will only be of benefit if the lang functions are declared to
   take the same primitive argument types.")

(declare recache)

(defn genome
  [inputs nodes out-idx lang options]
  (let [gm {:inputs (vec inputs)
            :nodes (vec nodes)
            :out-idx (vec out-idx)
            :lang lang
            :options options}]
    (recache gm)))

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
        fn-nodes (map #(rand-node % lang options) (range n-in size))
        out-idx (repeatedly n-out #(rand-nth (range n-in size)))]
    (genome inputs (concat in-nodes fn-nodes) out-idx lang options)))

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

(defn genome->expr
  "Converts a genome into a quoted function expression.
   This is like a macro, but at runtime."
  [{:as gm :keys [nodes out-idx inputs options]}]
  (let [data-type (:data-type options nil)
        size (count nodes)
        n-in (count inputs)
        active (sort (seq (active-idx gm)))
        syms (mapv #(symbol (str "nd-" % "_")) (range size))
        args (subvec syms 0 n-in)
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
                 (let [nd (nth nodes i)
                       in-idx (map (partial - i) (:in nd))
                       form (if-let [f (:fn nd)]
                              (list* f (map (partial nth syms) in-idx))
                              (:value nd))]
                   (recur (into lets [(nth syms i) form])
                          (next more)))
                 ;; done
                 lets))
        outs (mapv (partial nth syms) out-idx)]
    `(fn ~args (let ~lets ~outs))))

(defn function
  "Returns a function corresponding to the genome, possibly cached.
   This is preferred to `genome-outputs` as the function, once
   compiled, will be much faster to evaluate."
  [gm]
  (if-let [f (::function (meta gm))]
    f
    (eval (genome->expr gm))))

(defn recache
  "Clears any cached compiled function and, if option :precompile? is
   not false, compiles a new one. If option :recache-test? is not
   false, the genome's evaluation expression (using active nodes) is
   compared to the cached one to see whether recompilation is
   necessary."
  [{:as gm :keys [nodes out-idx inputs lang options]}]
  (if (:precompile? options true)
    (let [expr (genome->expr gm)]
      (if (and (:recache-test? options true)
               (= expr (::expr (meta gm))))
        gm
        ;; else - expression changed, recompile
        (vary-meta gm assoc
                   ::function (eval expr)
                   ::expr expr)))
    ;; else
    (vary-meta gm dissoc ::function ::expr)))

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
    (-> (assoc gm :nodes nds :out-idx oi)
        recache)))
