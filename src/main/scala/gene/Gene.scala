package gene

object Gene {
  type Individual = List[Int]
  type Population = List[Individual]

  def main(args: Array[String]): Unit = {
    start()
  }

  def randArgs(max: Int, min: Int)(n: Int): State[Long, List[(Boolean, Int)]] =
    State.sequence(List.fill(n)(State.both(RNG2.bool, RNG2.int(max, min))))

  def mutation(maxGene: Int, minGene: Int)(population: Population)/*: State[Long, Population]*/ = for {
    args <- randArgs(maxGene, minGene)(population.length)
  } yield ???

  def mutationIndividual(isNeedMutate: Boolean, gene: Int)(individual: Individual): Individual =
    ???

  def start(): Unit = {
    val board1 = List(7, 4, 1, 7, 8, 4, 2, 4)
    val board2 = List(7, 6, 1, 3, 8, 1, 8, 4)
    val board3 = List(7, 2, 1, 3, 1, 1, 8, 1)
    val board4 = List(7, 2, 1, 6, 8, 2, 1, 1)

    // выйгрышная
    val solutionBoard = List(7, 3, 1, 6, 8, 5, 2, 4)

    val population = List(board1, board2, board3, board4)
    val estimator = new FirstEstimator
    val rng = RNG(0)
    val crossover = Mutator.crossover _
    val mutation = Mutator.firstTaskMutation

    val mutator = crossover

    val result = Resolver.resolve(population, mutator, estimator, rng)
    val a = 1
  }
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
