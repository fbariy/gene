package gene

import gene.Gene.{Board, Population}

object Selector {
  def selection(estPop: List[(Board, Int)], rng: RNG): (Population, RNG) = {
    val half = estPop.size / 2
    val betterPop = estPop.sortWith((left, right) => left._2 > right._2).take(half).map(pair => pair._1)
    val restHalf = estPop.size - half
    val (arbitraryPop, newRng) = getArbitrary(estPop.map(pair => pair._1), restHalf, rng)
    (betterPop ++ arbitraryPop, newRng)
  }

  //@todo: fix seed in RNG
  def getArbitrary(pop: Population, arbitrarySize: Int, rng: RNG): (Population, RNG) =
    if (pop.isEmpty || arbitrarySize == 0) (List(), rng)
    else {
      val (result, currentRng) = getArbitrary(pop, arbitrarySize - 1, rng)
      val (index, nextRng) = currentRng.nextInt(pop.size - 1)
      (pop(index) :: result, nextRng)
    }
}