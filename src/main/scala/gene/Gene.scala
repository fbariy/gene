package gene

object Gene {
  type Board = List[Shape]
  type Population = List[Board]

//  @todo: выйгрышная доска
//  val board = List(
//    Shape(7, 1),
//    Shape(3, 2),
//    Shape(1, 3),
//    Shape(6, 4),
//    Shape(8, 5),
//    Shape(5, 6),
//    Shape(2, 7),
//    Shape(4, 8)
//  )


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

    val result = Resolver.resolve(List(board, board2, board3), new FirstEstimator, RNG(1))
    val a = 10
  }
}