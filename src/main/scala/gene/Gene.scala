package gene

import RNG._
import State._

import scala.annotation.tailrec

/*
 * * кроссовер возвращает не популяцию а новую особь на основе двух
 *
 *
 * на каждой итерации нахожим среднее арифм. генотипа популяции
 *
 * если среднее < 24 то динам. путь:
 * - проходим по всей популяции
 * - выбираем случайным образом 4 особи
 * - из них еще 2 с максимальной оценкой
 * - делаем двойной кроссовер выбранных 2-х (если особь 5 генов, то генерим от 1 до 4)
 * - делаем полную мутацию (шанс мутации - 1 к 10)
 * - резельтирующая особь есть результат итерации по популяции
 *
 * иначе:
 * - сортируем популяцию по убыванию
 * - проходим по популяции с шагом 2
 * - делаем кроссовер пары
 * - делаем мутацию результирующей особи кроссовера (вероятность 1 к 15)
 * - таким образом получаем 2 особи потомка
 * - дальше сравниваем 2 особи родителей и потомков, кто больше тот и остается в популяции
 */

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

    val population = List(
      board1,
      board2,
      board3,
      board4,
      board1,
      board2,
      board3,
      board4,
      board1,
      board2,
      board3,
      board4,
      board1,
      board2,
      board3,
      board4,
    )
    val estimator = new FirstEstimator

    val result = resolve(8, 1)(population, estimator)(0, 10000000)
    val temp = 10
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

  case class Result(solution: Option[Individual], iteration: Int, seed: Long)

  def resolve(maxGene: Int, minGene: Int)
             (population: Population, estimator: Estimator)
             (seed: Long, maxIter: Int): Result =
    resolveIter(population, estimator, maxIter, Result(None, 0, seed))(mutator(maxGene, minGene)(estimator))

  @tailrec
  def resolveIter(population: Population, estimator: Estimator, maxIter: Int, accum: Result)
                 (mutator: Population => State[Long, PopulationEstimation]): Result = {
    if (maxIter <= 0) accum
    else {
      val (nextSeed, result) = mutator(population).run(accum.seed)

      result.find(estimator.isSolution) match {
        case None => resolveIter(population, estimator, maxIter - 1, Result(None, accum.iteration + 1, nextSeed))(mutator)
        case Some((solution, _)) => Result(Some(solution), accum.iteration, nextSeed)
      }
    }
  }

  def mutator(maxGene: Int, minGene: Int)(estimator: Estimator)(population: Population): State[Long, PopulationEstimation] =
    for {
      mutationResult <- mutation(maxGene, minGene)(population)
      crossoverResult <- crossover(mutationResult)
      selectionResult <- selection(estimator)(crossoverResult)
    } yield selectionResult
}
