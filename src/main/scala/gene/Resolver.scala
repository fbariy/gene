package gene

import gene.Gene.{Individual, Population}

import scala.annotation.tailrec

object Resolver {
  @tailrec
  final def resolve(pop: Population, mutator: ((Population, RNG)) => (Population, RNG), est: Estimator, rng: RNG): Option[(Individual, RNG)] = {
    val estPop = pop.map(individual => (individual, est.estimate(individual)))
    val solution = estPop.find(pair => est.isSolution(pair._2))

    solution match {
      case Some((board, _)) => Some(board -> rng)
      case None =>
        if (false /* todo: add breaker resolve */) None
        else {
          val (selectionPop, selectionRNG) = Selector.selection(estPop, rng)
          val (mutationPop, mutationRNG) = mutator((selectionPop, selectionRNG))
          resolve(mutationPop, mutator, est, mutationRNG)
        }
    }
  }
}