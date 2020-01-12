package gene

case class RNG(seed: Long) {
  def nextInt(): (Int, RNG) = {
    val (number, rng) = nextLong()
    (number.toInt, rng)
  }

  def nextLong(): (Long, RNG) = {
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
    val nextRNG = RNG(newSeed)
    (newSeed >>> 16, nextRNG)
  }

  def nextInt(max: Int, min: Int = 0): (Int, RNG) = {
    val (number, rng) = nextLong()
    val numberWithinBounds = (number % max + min).toInt
    (numberWithinBounds, rng)
  }
}
