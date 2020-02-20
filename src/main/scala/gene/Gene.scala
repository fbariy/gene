package gene

import RNG2._
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

    val result = resolver(1, List(board1, board2, board3, board5), new FirstEstimator)
    val temp = 5
  }

  def crossoverIndividual(maxGene: Int, minGene: Int)(left: Individual, right: Individual): State[Long, Population] =
    int(maxGene, minGene).map { point =>
      val (leftLeft, leftRight) = left.splitAt(point)
      val (rightLeft, rightRight) = right.splitAt(point)
      List(leftLeft ++ rightRight, rightLeft ++ leftRight)
    }

  def crossover(maxGene: Int, minGene: Int)(population: Population): State[Long, Population] =
    sequence(population
      .sliding(2, 2)
      .map { pair => crossoverIndividual(maxGene, minGene)(pair.head, pair.last) }
      .toList)
      .map(_.flatten)

  def mutation(maxGene: Int, minGene: Int)(population: Population): State[Long, Population] =
    sequence(population.map { individual => mutationIndividual(maxGene, minGene)(individual) })

  def mutationIndividual(maxGene: Int, minGene: Int)(individual: Individual): State[Long, Individual] = for {
    isNeedMutate <- bool
    gene <- int(maxGene, minGene)
    position <- lessThan(individual.size)
  } yield
    if (isNeedMutate) individual.updated(position, gene)
    else individual

  def mutationFirst: Population => State[Long, Population] = mutation(8, 1)
  def crossoverFirst: Population => State[Long, Population] = crossover(8, 1)

  @tailrec
  def resolver(seed: Long, population: Population, estimator: Estimator): Individual = {
    val (nextSeed, result) = mutator(population, estimator).run(seed)
    val maybeSolution = result.find(individual => estimator.isSolution(individual._2))

    maybeSolution match {
      case None => resolver(nextSeed, result.map(_._1), estimator)
      case Some((solution, _)) => solution
    }
  }

  def mutator(population: Population, estimator: Estimator): State[Long, PopulationEstimation] = for {
    mutationResult <- mutationFirst(population)
    crossoverResult <- crossoverFirst(mutationResult)
    //@todo: add selector
  } yield crossoverResult.map(individual => (individual, estimator.estimate(individual)))
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
