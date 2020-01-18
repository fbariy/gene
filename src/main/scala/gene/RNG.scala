package gene

case class RNG(seed: Long) {
  def nextLong(): (Long, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val newRng = RNG(newSeed)
    (newSeed >>> 16, newRng)
  }

  def nextInt(): (Int, RNG) = nextLong() match {
    case (number, rng) => (number.toInt, rng)
  }

  def nextInt(max: Int, min: Int = 0): (Int, RNG) = nextLong() match {
    case (number, rng) => ((number % max + min).toInt, rng)
  }

  def nextBool(): (Boolean, RNG) = nextInt(1) match {
    case (1, nextRNG) => (true, nextRNG)
    case (2, nextRNG) => (false, nextRNG)
  }
}
