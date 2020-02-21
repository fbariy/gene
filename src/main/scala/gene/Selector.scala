package gene

import gene.Gene.{Individual, Population}

import scala.annotation.tailrec

object Selector {
  def selection (estPop: List[(Individual, Int)], rng: RNG): (Population, RNG) = {
    val half = estPop.size / 2
    val betterPop = estPop.sortWith((left, right) => left._2 > right._2).take(half).map(pair => pair._1)
    val restHalf = estPop.size - half
    val (arbitraryPop, newRng) = getArbitrary(estPop.map(pair => pair._1), restHalf, rng)
    (betterPop ++ arbitraryPop, newRng)
  }

  def selection(estimator: Estimator)(population: Population): State[Long, Population] = {
    val estimationPopulation = population.zip { population.map(estimator.estimate) }
    val half = estimationPopulation.size / 2
    val betterPop = estimationPopulation.sortWith((left, right) => left._2 > right._2).take(half).map(pair => pair._1)
    val restHalf = estimationPopulation.size - half
    val (arbitraryPop, newRng) = getArbitrary(population, restHalf)
    (betterPop ++ arbitraryPop, newRng)
    ???
  }


  def getArbitrary(population: Population, arbitratySize: Int): State[Long, Population] = ???

  def getArbitrary(population: Population, arbitrarySize: Int, rng: RNG): (Population, RNG) =
    if (population.isEmpty || arbitrarySize == 0) (List(), rng)
    else {
      val (result, currentRng) = getArbitrary(population, arbitrarySize - 1, rng)
      val (index, nextRng) = currentRng.nextIntLessThen(population.size - 1)
      (population(index) :: result, nextRng)
    }
}