import Gene.{Board, Population}

import scala.util.Random

object Gene {
  case class Shape(x: Int, y: Int)

  type Board = List[Shape]
  type Population = List[Board]


  def main(args: Array[String]): Unit = {
    val board = List(
      Shape(7, 1),
      Shape(3, 2),
      Shape(1, 3),
      Shape(6, 4),
      Shape(8, 5),
      Shape(5, 6),
      Shape(2, 7),
      Shape(4, 8)
    )
  }

  //@todo: рефакторинг на нормальную генерацию
  def generate(n: Int, acc: Vector[Int] = Vector()): Vector[Int] = {
    if (acc.length == n) acc
    else generate(n, acc :+ suitableRand(n, acc))
  }

  def suitableRand(n: Int, generated: Vector[Int]): Int = {
    val randVal = rand(n)

    if (generated.contains(randVal)) suitableRand(n, generated)
    else randVal
  }

  def rand(max: Int, min: Int = 1): Int = min + Random.nextInt(max)

  // есть N особей
  // оцениваем каждую особь
  // выбираем N / 2 лучших особей
  // остальные берем случайным образом
  // мутируем особи "как-то"
  // смотрим на результат оценки
  // оцениваем, если оценка удовл. задаче - конец иначе по новой...
}

class Selector {
  //@todo: добавить рандомайзер
  def selection(population: Population, estimator: Estimator): Either[Board, Population] = {

  }
}
