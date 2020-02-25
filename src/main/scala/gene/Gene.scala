package gene

import RNG._
import State._
import Generator._

import scala.io.StdIn.readLine
import scala.annotation.tailrec

object Gene {
  type Individual = List[Int]
  type Population = List[Individual]
  type Estimation = (Individual, Int)
  type PopulationEstimation = List[Estimation]

  case class Result(solution: Option[Individual], iteration: Int, seed: Long)

  def main(args: Array[String]): Unit = {
    print("Hi. Enter the population size: ")
    val populationSize = readLine().toInt

    print("Enter the n: ")
    val n = readLine().toInt

    print("Enter the max iteration amount: ")
    val maxIter = readLine().toInt

    print("Enter the seed: ")
    val seed = readLine().toInt

    val est = new NRookEstimator(n)
    val gen = generator(populationSize)(n, 1)(n)

    val result = resolve(n, 1, 24)(est, gen)(seed, maxIter)

    println(s"\nIteration amount: ${result.iteration}")
    println(s"Final seed: ${result.seed}")

    result.solution match {
      case Some(individual) => println(s"Solution individual: " + individual)
      case None => println(s"Solution not found, retry with other seed or add max iteration amount")
    }
  }


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


  def resolve(maxGene: Int, minGene: Int, average: Int)
             (estimator: Estimator, generator: State[Long, Population])
             (seed: Long, maxIter: Int): Result = {
    val (nextSeed, population) = generator.run(seed)

    resolveIter(
      dynamicMutatorPopulation(maxGene, minGene)(estimator),
      conservativeMutatorPopulation(maxGene, minGene)(estimator)
    )(population, estimator, average)(maxIter, Result(None, 0, nextSeed))
  }


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
