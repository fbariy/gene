trait Estimator {
  type Board
  def estimate(board: Board): Int
}

class FirstEstimator extends Estimator {
  type Board = List[Shape]

  def estimate(board: Board): Int = {
    def compute(current: Shape, rest: Board) = {
      val operator: (Int, Shape) => Int = (acc, other) => if (collise(current, other)) acc else acc + 1
      rest.foldLeft(0)(operator)
    }

    def collise(current: Shape, other: Shape) = {
      current.x == other.x ||
      current.y == other.y ||
      Math.abs(other.x - current.x) == Math.abs(other.y - current.y)
    }

    def estimateIter(board: Board, acc: Int): Int = board match {
      case Nil => acc
      case head :: tail => estimateIter(tail, acc + compute(head, tail))
    }
    estimateIter(board, 0)
  }
}
