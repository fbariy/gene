package gene

import RNG._
import State._

import scala.annotation.tailrec

object Gene {
  type Individual = List[Int]
  type Population = List[Individual]
  type Estimation = (Individual, Int)
  type PopulationEstimation = List[Estimation]

  def main(args: Array[String]): Unit = {
    // выйгрышная
    val solutionBoard = List(7, 3, 1, 6, 8, 5, 2, 4)

    val board1 = List(7, 4, 1, 7, 8, 4, 2, 4)
    val board2 = List(7, 6, 1, 3, 8, 1, 8, 4)
    val board3 = List(7, 2, 1, 3, 1, 1, 8, 1)
    val board4 = List(7, 2, 1, 6, 8, 2, 1, 1)
    val board5 = List(7, 4, 1, 7, 8, 4, 2, 4)

    val population = List(board1, board2, board3, board4, board5)
    val estimator = new FirstEstimator

    val result = resolve(1, population, estimator)
  }

  def crossover(population: Population): State[Long, Population] =
    sequence(population
      .sliding(2, 2)
      .map { pair => crossoverIndividual(pair.head, pair.last) }
      .toList)
      .map(_.flatten)

  def crossoverIndividual(left: Individual, right: Individual): State[Long, Population] =
    lessThan(left.size).map { point =>
      val (leftLeft, leftRight) = left.splitAt(point)
      val (rightLeft, rightRight) = right.splitAt(point)
      List(leftLeft ++ rightRight, rightLeft ++ leftRight)
    }


  def mutation(maxGene: Int, minGene: Int)(population: Population): State[Long, Population] =
    sequence(population map { mutationIndividual(maxGene, minGene)(_) })

  def mutationIndividual(maxGene: Int, minGene: Int)(individual: Individual): State[Long, Individual] =
    for {
      isNeedMutate <- bool
      gene <- int(maxGene, minGene)
      position <- lessThan(individual.size)
    } yield
      if (isNeedMutate) individual.updated(position, gene)
      else individual


  def selection(estimator: Estimator)(population: Population): State[Long, PopulationEstimation] = {
    val estimated = estimator.estimate(population)

    val selected = estimated
      .sortWith((current, next) => current._2 > next._2)
      .take(estimated.size / 2)

    chooseArbitraryIndividuals(estimated.size - selected.size)(population)
      .map(estimator.estimate)
      .map(selected ++ _)
  }


  def chooseArbitraryIndividuals(individualsCount: Int)(population: Population): State[Long, Population] =
    sequence(List.fill(individualsCount)(lessThan(population.size).map(population(_))))

  //@todo: replace
  def mutationFirst: Population => State[Long, Population] = mutation(8, 1)

  @tailrec
  def resolve(seed: Long, population: Population, estimator: Estimator): Individual = {
    val (nextSeed, result) = mutator(population, estimator).run(seed)
    val maybeSolution = result.find(estimator.isSolution)

    //@todo: add statistic for every iteration

    maybeSolution match {
      case None => resolve(nextSeed, result.map(_._1), estimator)
      case Some((solution, _)) => solution
    }
  }

  def mutator(population: Population, estimator: Estimator): State[Long, PopulationEstimation] =
    for {
      mutationResult <- mutationFirst(population)
      crossoverResult <- crossover(mutationResult)
      selectionResult <- selection(estimator)(crossoverResult)
    } yield selectionResult
}

/**
  * Подзадачи:
  *
  * 1 Мутаторы
  * [1.1] Плохая читаемость из-за RNG
  * [1.2] Принимает пару вместо нескольких аргументов
  * 1.3 Общий рефакторинг
  * [1.4] Покрытие тестами
  *
  * 2 Прерыватель ризолвера
  *
  * 3 Генератор первой популяции
  *
  * 4 Статистика поиска решения
  *
  * 5 Визуализация результата
  *
  * 6 Задание по варианту
  */
