package gene

import gene.Gene.Individual

object Mutator {
  def crossover(first: Individual, second: Individual, point: Int): (Individual, Individual) = {
    val (firstLeft, firstRight) = first.splitAt(point)
    val (secondLeft, secondRight) = second.splitAt(point)
    (firstLeft ++ secondRight, secondLeft ++ firstRight)
  }
}