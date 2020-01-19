package gene

case class RNG(seed: Long) {
  def nextLong(): (Long, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = RNG(newSeed)
    (newSeed >>> 16, nextRNG)
  }

  def nextInt(): (Int, RNG) = nextLong() match {
    case (number, rng) => (number.toInt, rng)
  }

  def nextInt(max: Int, min: Int = 0): (Int, RNG) = nextLong() match {
    case (number, rng) => ((min + number % (max + 1 - min)).toInt, rng)
  }

  def nextIntLessThen(lessThen: Int, min: Int = 0): (Int, RNG) = nextInt(lessThen, min) match {
    case (number, rng) => if (number - 1 >= min) (number - 1, rng) else (number, rng)
  }

  def nextBool(): (Boolean, RNG) = nextInt(1) match {
    case (1, rng) => (true, rng)
    case (_, rng) => (false, rng)
  }
}
