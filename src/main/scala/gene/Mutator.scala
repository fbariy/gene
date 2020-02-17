package gene

import gene.Gene.{Individual, Population}

object Mutator {
  private def crossover(first: Individual, second: Individual, point: Int): (Individual, Individual) = {
    val (firstLeft, firstRight) = first.splitAt(point)
    val (secondLeft, secondRight) = second.splitAt(point)
    (firstLeft ++ secondRight, secondLeft ++ firstRight)
  }

  def crossover(pair: (Population, RNG)): (Population, RNG) = pair match {
    case (pop, rng) =>
      if (0 == pop.size % 2) crossoverSlider(pop, rng)
      else {
        val (newPop, newRNG) = crossoverSlider(pop.init, rng)
        (newPop :+ pop.last, newRNG)
      }
  }

  def crossoverSlider(pop: Population, rng: RNG): (Population, RNG) = pop.sliding(2).flatMap()

//  def crossover(pair: (Population, RNG)): (Population, RNG) = {
//    val (pop, rng) = pair
//    pop match {
//      case first :: second :: rest =>
//        val (number, nextRng) = rng.nextIntLessThen(first.size)
//        val (left, right) = crossover(first, second, number)
//        val (resultPop, resultRng) = crossover((rest, nextRng))
//        val newPop = List(left, right) ++ resultPop
//        (newPop, resultRng)
//      case first :: _ => (List(first), rng)
//      case List() => (List(), rng)
//    }
//  }

  def mutation(minGene: Int, maxGene: Int)(pair: (Population, RNG)) = {
    def mutationIter(pair: (Population, RNG)): (Population, RNG) = {
      val (pop, rng) = pair
      pop match {
        case List() => (List(),  rng)
        case first :: rest =>
          //@todo: refactor
          val mutatedIndividual = rng.nextBool() match {
            case (false, switchRNG) => (first, switchRNG)
            case (true, switchRNG) =>
              val (newIndividual, mutateRNG) = mutate(minGene, maxGene)(switchRNG, first)
              (newIndividual, mutateRNG)
          }

          val (mutated, mutateRNG) = mutatedIndividual
          val (newPop, mutatePopRNG) = mutationIter((rest, mutateRNG))
          (mutated :: newPop, mutatePopRNG)
      }
    }

    def mutate(minGene: Int, maxGen: Int)(rng: RNG, individual: Individual): (Individual, RNG) = {
      val (point, pointRNG) = rng.nextIntLessThen(individual.size)
      val (gene, geneRNG) = pointRNG.nextInt(maxGen, minGene)
      (individual.updated(point, gene), geneRNG)
    }

    mutationIter(pair)
  }

  def firstTaskMutation: ((Population, RNG)) => (Population, RNG) = mutation(1, 8)
}