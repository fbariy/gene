package gene

object Gene {
  type Individual = List[Int]
  type Population = List[Individual]

  def main(args: Array[String]): Unit = {
    start()
  }

  def start(): Unit = {
    val board1 = List(7, 4, 1, 7, 8, 4, 2, 4)
    val board2 = List(7, 6, 1, 3, 8, 1, 8, 4)
    val board3 = List(7, 2, 1, 3, 1, 1, 8, 1)
    val board4 = List(7, 2, 1, 6, 8, 2, 1, 1)

    // выйгрышная
    val solutionBoard = List(7, 3, 1, 6, 8, 5, 2, 4)

    val pop = List(board1, board2, board3, board4)
    val est = new FirstEstimator
    val rng = RNG(0)
    val crossover = Mutator.crossover _
    val mutation = Mutator.firstTaskMutation

    val result = Resolver.resolve(pop,  est, rng)
    val a = 1
  }
}
