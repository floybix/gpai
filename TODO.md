# TODO

* lang as 3-tuples [symbol return-type [arg-types]]
  * possible extension with weighting?

* protocol for evolvable genomes (cgp, icgp, tree)
  * mutate
  * crossover / fuse
  * active-ids
  * genome->expr
    * function
  * expression (simplify)

* impl/ vs api namespaces

* pre / post / assertions

* use expresso to simplify evolved arithmetic expressions
  * specify rules for language sets

* datomic storage of population histories?


## methods

* empirically compare icgp to cgp

* icgp
  * fuse / crossover (merge nodes, crossover out-ids)
    * then discard neutral nodes to return to target genome size
  * perf
    * primitive ops

* ecgp

* cgp
  * consistency with icgp
    * node last-used accounting?
    * constants
  * adopt dominant strategies as global function codebases?
  * automatically defined _closures_ (override some args only)

* island populations

* mutation or optimisation to adjust numeric constants


## problems

* santa fe trail ant
  * memoize fitness

* random number generators
  * visualise evolution progress
  * dominance tournament

* three-way coevolution:
  * overseer / goodies / baddies
  * challenge + response
  * (generalise coevolve?)

* game of life vs death
  * game of life, torus
  * divide initial grid between life vs death
  * if any non-repetitive activity after N steps, life wins

* spikes

* sumobots

* submarine?

* learning
  * hunt the wumpus
