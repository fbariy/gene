package gene

import RNG._
import State._

import scala.annotation.tailrec

object Gene {
  type Individual = List[Int]
  type Population = List[Individual]
  type Estimation = (Individual, Int)
  type PopulationEstimation = List[Estimation]


  def conservativeMutatorPopulation(maxGene: Int, minGene: Int)
                                   (estimator: Estimator)
                                   (population: PopulationEstimation): State[Long, Population] =
    sequence(population
      .sortWith((current, next) => current._2 > next._2)
      .map(_._1)
      .sliding(2, 2)
      .map { conservativeMutator(maxGene, minGene)(estimator, _) }
      .toList)
      .map(_.flatten)


  def conservativeMutator(maxGene: Int, minGene: Int)
                         (estimator: Estimator, population: Population): State[Long, Population] =
    for {
      crossoverFirstChild <- crossover(population.head, population.last)
      mutationFirstChild <- mutation(maxGene, minGene)(15)(crossoverFirstChild)
      crossoverSecondChild <- crossover(population.head, population.last)
      mutationSecondChild <- mutation(maxGene, minGene)(15)(crossoverSecondChild)

      childPopulation = List(mutationFirstChild, mutationSecondChild)
      parentPopulation = List(population.head, population.last)
    } yield
      if (estimator.estimateAverage(childPopulation) > estimator.estimateAverage(parentPopulation)) childPopulation
      else parentPopulation

  def dynamicMutator(maxGene: Int, minGene: Int)
                    (estimator: Estimator, population: Population): State[Long, Individual] =
    for {
      slice <- chooseArbitraryIndividuals(4)(population)
      maxPair = estimator
        .estimate(slice)
        .sortWith((current, next) => current._2 > next._2)
        .take(slice.size / 2)
        .map(_._1)
      crossover <- crossoverTwice(maxPair.head, maxPair.last)
      mutation <- mutation(maxGene, minGene)(10)(crossover)
    } yield mutation

  def dynamicMutatorPopulation(maxGene: Int, minGene: Int)(estimator: Estimator)(population: Population): State[Long, List[Individual]] =
    sequence(List.fill(population.size)(dynamicMutator(maxGene, minGene)(estimator, population)))

  def main(args: Array[String]): Unit = {
    // выйгрышная
    val solutionBoard = List(7, 3, 1, 6, 8, 5, 2, 4)

    val board1 = List(7, 4, 1, 7, 8, 4, 2, 4)
    val board2 = List(9, 6, 1, 3, 8, 1, 8, 4)
    val board3 = List(3, 2, 1, 3, 1, 1, 8, 1)
    val board4 = List(2, 2, 1, 6, 8, 2, 1, 1)
    val board5 = List(8, 4, 1, 7, 8, 4, 2, 4)

    val population = List(
      board1,
      board2,
      board3,
      board4,
      board5,
      board1,
      board2,
      board3,
      board4,
      board5,
      board1,
      board2,
      board3,
      board4,
      board5,
      board1,
      board2,
      board3,
      board4,
      board5,
      board1,
      board2,
      board3,
      board4,
      board5,
    )
    val estimator = new FirstEstimator

    // solution seed: 4
    val result = resolve(8, 1, 24)(population, estimator)(4, 500000)

    val temp = 10
  }


  def crossover(left: Individual, right: Individual): State[Long, Individual] =
    lessThan(left.size - 1, 1).map(point => right.splitAt(point)._1 ++ left.splitAt(point)._2)

  def crossoverTwice(left: Individual, right: Individual): State[Long, Individual] =
    for {
      once <- crossover(left, right)
      twice <- crossover(right, once)
    } yield twice

  def mutation(maxGene: Int, minGene: Int)(chanceMutation: Int)(individual: Individual): State[Long, Individual] =
    for {
      isNeedMutate <- bool(chanceMutation)
      gene <- int(maxGene, minGene)
      position <- lessThan(individual.size)
    } yield
      if (isNeedMutate) individual.updated(position, gene)
      else individual

  def chooseArbitraryIndividuals(individualsCount: Int)(population: Population): State[Long, Population] =
    sequence(List.fill(individualsCount)(lessThan(population.size).map { population(_) }))

  case class Result(solution: Option[Individual], iteration: Int, seed: Long)

  def resolve(maxGene: Int, minGene: Int, average: Int)
             (population: Population, estimator: Estimator)
             (seed: Long, maxIter: Int): Result =
    resolveIter(
      dynamicMutatorPopulation(maxGene, minGene)(estimator),
      conservativeMutatorPopulation(maxGene, minGene)(estimator)
    )(
      population,
      estimator,
      average)(
      maxIter,
      Result(None, 0, seed)
    )

  @tailrec
  def resolveIter(dynamic: Population => State[Long, Population], conservative: PopulationEstimation => State[Long, Population])
                 (population: Population, estimator: Estimator, average: Int)
                 (maxIter: Int, accum: Result): Result = {
    // too much attempts
    if (maxIter <= 0) accum
    else {
      val estimated = estimator.estimate(population)
      val solution = estimated.find(estimator.isSolution)

      solution match {
        // solution is found
        case Some((solution, _)) => Result(Some(solution), accum.iteration, accum.seed)
        // solution not found
        case None =>
            // choose mutation strategy
            if (estimator.estimationAverage(estimated) < average) {
              val (nextSeed, nextPopulation) = dynamic(population).run(accum.seed)
              resolveIter(dynamic, conservative)(nextPopulation, estimator, average)(maxIter - 1, Result(None, accum.iteration + 1, nextSeed))
            }

            else {
              val (nextSeed, nextPopulation) = conservative(estimated).run(accum.seed)
              resolveIter(dynamic, conservative)(nextPopulation, estimator, average)(maxIter - 1, Result(None, accum.iteration + 1, nextSeed))
            }
      }
    }
  }
}
