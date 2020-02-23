package gene

object RNG {
  def int: State[Long, Int] =
    State(seed => {
      val newSeed = (seed * 0x5DDEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val n = newSeed >>> 16
      (newSeed, n.toInt)
    })

  def double: State[Long, Double] =
    int.map { a =>
      (if (a == Int.MaxValue) a - 1 else a).toDouble.abs / Int.MaxValue
    }

  def bool: State[Long, Boolean] = int.map(_ > 0)

  def int(max: Int, min: Int = 0): State[Long, Int] =
    double.map { a =>
      // borrowed from https://javarush.ru/groups/posts/1256-generacija-sluchaynogo-chisla-v-zadannom-diapazone
      (a * (max - min + 1) + min).toInt
    }

  def lessThan(lessThan: Int, min: Int = 0): State[Long, Int] =
    int(if (lessThan - 1 <= min) lessThan else lessThan - 1, min)
}