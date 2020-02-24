package gene

import gene.Gene._
import scala.annotation.tailrec

trait Estimator {
  def isSolution(estimation: Int): Boolean

  def isSolution(estimation: Estimation): Boolean = isSolution(estimation._2)

  def estimate(individual: Individual): Int

  def estimate(population: Population): PopulationEstimation = population zip ( population map estimate )

  def estimateAverage(population: Population): Int = population.foldLeft(0)(_ + estimate(_)) / population.size

  def estimationAverage(estimation: PopulationEstimation): Int = estimation.foldLeft(0)(_ + _._2) / estimation.size
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