package gene

object Gene {
  type Individual = List[Int]
  type Population = List[Individual]

  //@todo: не правильно работает, позиция гена остается прежней
  def crossoverPop(pop: Population, rng: RNG): (Population, RNG) = {
    def crossover(first: Individual, second: Individual, point: Int): (Individual, Individual) = {
      val (firstLeft, firstRight) = first.splitAt(point)
      val (secondLeft, secondRight) = second.splitAt(point)
      (firstLeft ++ secondRight, secondLeft ++ firstRight)
    }

    pop match {
      case first :: second :: rest =>
        val (number, nextRng) = rng.nextInt(first.size)
        val (left, right) = crossover(first, second, number)
        val (resultPop, resultRng) = crossoverPop(rest, nextRng)
        val newPop = List(left, right) ++ resultPop
        (newPop, resultRng)

      case first :: _ => (List(first), rng)

      case List() => (List(), rng)
    }
  }

  def main(args: Array[String]): Unit = {
    val board = List(
      Shape(7, 1),
      Shape(4, 2),
      Shape(1, 3),
      Shape(7, 4),
      Shape(8, 5),
      Shape(4, 6),
      Shape(2, 7),
      Shape(4, 8)
    )
    val board2 = List(
      Shape(7, 1),
      Shape(6, 2),
      Shape(1, 3),
      Shape(3, 4),
      Shape(8, 5),
      Shape(1, 6),
      Shape(8, 7),
      Shape(4, 8)
    )
    val board3 = List(
      Shape(7, 1),
      Shape(2, 2),
      Shape(1, 3),
      Shape(3, 4),
      Shape(1, 5),
      Shape(1, 6),
      Shape(8, 7),
      Shape(1, 8)
    )

    val board4 = List(
      Shape(7, 1),
      Shape(2, 2),
      Shape(1, 3),
      Shape(6, 4),
      Shape(8, 5),
      Shape(2, 6),
      Shape(1, 7),
      Shape(1, 8)
    )
    // выйгрышная
    val solutionBoard = List(
      7,
      3,
      1,
      6,
      8,
      5,
      2,
      4
    )
  }
}

/**
  * +++ Написать crossover метод (Population, RNG) -> Population
  * Написать mutation метод (Population, RNG) -> Population
  */