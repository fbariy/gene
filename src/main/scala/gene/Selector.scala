package gene

import gene.Gene.{Individual, Population, PopulationEstimation}

object Selector {
  def selection(estimator: Estimator)(population: Population): State[Long, PopulationEstimation] = {
    val estimated = population.zip { population.map(estimator.estimate) }
    val half = estimated.size / 2
    val selected = estimated
      .sortWith((current, next) => current._2 > next._2)
      .take(half)

    getArbitrary(estimated.size - half)(population)
      .map(_.map(individual => (individual, estimator.estimate(individual))))
      .map(selected ++ _)
  }


  def getArbitrary(individualSize: Int)(population: Population): State[Long, Population] =
    State.sequence(List.fill(individualSize)(RNG2.lessThan(population.size).map(population(_))))
}