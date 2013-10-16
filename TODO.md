# TODO


* protocol for evolvable genomes (cgp, icgp, tree)
  * mutate
  * crossover / fuse
  * active-ids
  * genome->expr
    * function
  * expression (simplify)


## methods

* icgp
  * globally unique ids: allows merging genomes!
  * generate input and constant nodes once, shared across popn
    * constant nodes should not be mutated

* ecgp

* cgp ~crossover/merge by swallowing other genome as a function codebase?
  * or adopt dominant strategies as global function codebases
  * automatically defined _closures_ (override some args only)

* island populations

* mutation or optimisation to adjust numeric constants


## misc

* cgp consistency with icgp
  * node last-used accounting?
  * constants

* pre / post / assertions

* use expresso to simplify evolved arithmetic expressions
  * specify rules for language sets


## problems

* santa fe trail ant

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
