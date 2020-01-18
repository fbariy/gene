package gene

import gene.Gene._
import scala.annotation.tailrec

trait Estimator {
  def isSolution(estimation: Int): Boolean
  def estimate(individual: Individual): Int
}

class FirstEstimator extends Estimator {

  def isSolution(estimation: Int): Boolean = estimation == 28

  def estimate(individual: Individual): Int = {
    @tailrec
    def estimateIter(individuals: List[(Int, Int)], acc: Int): Int = individuals match {
      case Nil => acc
      case head :: tail => estimateIter(tail, acc + tail.count(current => !collide(head, current)))
    }
    estimateIter(individual.zipWithIndex, 0)
  }

  private def collide(current: (Int, Int), other: (Int, Int)) = {
    val (currentX, currentY) = current
    val (otherX, otherY) = other

    currentX == otherX ||
      currentY == otherY ||
      Math.abs(otherX - currentX) == Math.abs(otherY - currentY)
  }
}