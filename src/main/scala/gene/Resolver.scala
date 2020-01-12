package gene

import gene.Gene.{Board, Population}

import scala.annotation.tailrec

object Resolver {
  @tailrec
  final def resolve(pop: Population, est: Estimator, rng: RNG): Option[Board] = {
    val estPop = pop.map(board => (board, est.estimate(board)))
    val solution = estPop.find(pair => est.isSolution(pair._2))
    solution match {
      case Some((board, _)) => Some(board)
      case None =>
        if (false /* todo: add breaker resolve */) None
        else {
          val (selectedPop, newRng) = Selector.selection(estPop, rng)


          /* todo: add some morphism to population */

          resolve(selectedPop, est, newRng)
        }
    }
  }
}