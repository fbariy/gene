package gene

object RNG {
  def int: State[Long, Int] =
    State(seed => {
      val newSeed = (seed * 0x5DDEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val n = newSeed >>> 16
      (newSeed, n.toInt)
    })

  def double: State[Long, Double] =
    int.map { a => (if (a == Int.MaxValue) a - 1 else a).toDouble.abs / Int.MaxValue }

  def bool(trueChance: Int = 2): State[Long, Boolean] =
    int(trueChance, 1).map(_ == 1)

  def nonNegative: State[Long, Int] =
    int.map { a => (if (a == Int.MinValue) a + 1 else a).abs }

  def nonNegative(n: Int): State[Long, Int] =
    nonNegative.flatMap { a =>
      val mod = a % n
      if (a + n - mod >= 0) State.unit(mod)
      else nonNegative(n)
    }

  def int(max: Int, min: Int = 0): State[Long, Int] =
    nonNegative(max).flatMap { a =>
      val result = a + min
      if (result <= max) State.unit(result)
      else int(max, min)
    }

  def lessThan(lessThan: Int, min: Int = 0): State[Long, Int] =
    int(if (lessThan - 1 <= min) lessThan else lessThan - 1, min)
}