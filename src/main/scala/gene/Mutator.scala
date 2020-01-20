package gene

import gene.Gene.{Individual, Population}

object Mutator {
  private def crossover(first: Individual, second: Individual, point: Int): (Individual, Individual) = {
    val (firstLeft, firstRight) = first.splitAt(point)
    val (secondLeft, secondRight) = second.splitAt(point)
    (firstLeft ++ secondRight, secondLeft ++ firstRight)
  }

  def crossover(rng: RNG)(pop: Population): (Population, RNG) = {
    pop match {
      case List() => List() -> rng
      case first :: _ => List(first) -> rng
      case first :: second :: rest =>
        val (number, nextRng) = rng.nextIntLessThen(first.size)
        val (left, right) = crossover(first, second, number)
        val (resultPop, resultRng) = crossover(nextRng)(rest)
        val newPop = List(left, right) ++ resultPop
        (newPop, resultRng)
    }
  }

  def mutation(minGene: Int, maxGene: Int)(rng: RNG)(pop: Population) = {
    def mutationIter(rng: RNG)(pop: Population): (Population, RNG) = pop match {
      case List() => List() -> rng
      case first :: rest =>
        //@todo: refactor
        val mutatedIndividual = rng.nextBool() match {
          case (false, switchRNG) => (first, switchRNG)
          case (true, switchRNG) =>
            val (newIndividual, mutateRNG) = mutate(minGene, maxGene)(switchRNG, first)
            (newIndividual, mutateRNG)
        }

        val (mutated, mutateRNG) = mutatedIndividual
        val (newPop, mutatePopRNG) = mutationIter(mutateRNG)(rest)
        (mutated :: newPop, mutatePopRNG)
    }

    def mutate(minGene: Int, maxGen: Int)(rng: RNG, individual: Individual): (Individual, RNG) = {
      val (point, pointRNG) = rng.nextIntLessThen(individual.size)
      val (gene, geneRNG) = pointRNG.nextInt(maxGen, minGene)
      (individual.updated(point, gene), geneRNG)
    }

    mutationIter(rng)(pop)
  }

  def firstTaskMutation = mutation(1, 8)
}