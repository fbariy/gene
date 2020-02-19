package gene

import RNG2._
import State._

object Gene {
  type Individual = List[Int]
  type Population = List[Individual]

  def main(args: Array[String]): Unit = {
    // выйгрышная
    val solutionBoard = List(7, 3, 1, 6, 8, 5, 2, 4)
  }

  def mutation(maxGene: Int, minGene: Int)(population: Population): State[Long, Population] =
    sequence(population.map(individual => mutationIndividual(maxGene, minGene)(individual)))

  def mutationIndividual(maxGene: Int, minGene: Int)(individual: Individual): State[Long, Individual] = for {
    isNeedMutate <- bool
    gene <- int(maxGene, minGene)
    position <- lessThan(individual.size)
  } yield
    if (isNeedMutate) individual.updated(position, gene)
    else individual
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
