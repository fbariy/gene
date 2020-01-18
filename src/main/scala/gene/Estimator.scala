package gene

import gene.Gene._
import scala.annotation.tailrec

trait Estimator {
  def isSolution(estimate: Int): Boolean
  def estimate(board: Individual): Int
}

class FirstEstimator extends Estimator {

  def isSolution(estimate: Int): Boolean = estimate == 28

  def estimate(board: Individual): Int = {
    @tailrec
    def estimateIter(board: Individual, acc: Int): Int = board match {
      case Nil => acc
      case head :: tail => estimateIter(tail, acc + compute(head, tail))
    }
    estimateIter(board, 0)
  }

  private def compute(current: Shape, rest: Individual) = {
    val operator: (Int, Shape) => Int = (acc, other) => if (collise(current, other)) acc else acc + 1
    rest.foldLeft(0)(operator)
  }

  private def collise(current: Shape, other: Shape) = {
    current.x == other.x ||
      current.y == other.y ||
      Math.abs(other.x - current.x) == Math.abs(other.y - current.y)
  }
}
