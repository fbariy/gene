package gene

import gene.Gene.{Individual, Population}

import scala.annotation.tailrec

object Resolver {
  @tailrec
  final def resolve(pop: Population, est: Estimator, rng: RNG): Option[Individual] = {
    val estPop = pop.map(board => (board, est.estimate(board)))
    val solution = estPop.find(pair => est.isSolution(pair._2))
    solution match {
      case Some((board, _)) => Some(board)
      case None =>
        if (false /* todo: add breaker resolve */) None
        else {
          val (selectedPop, newRng) = Selector.selection(estPop, rng)
          val (crossoverResult, mapRng) = Gene.crossoverPop(selectedPop, newRng)
          /* todo: add some morphism to population */

          resolve(crossoverResult, est, mapRng)
        }
    }
  }
}