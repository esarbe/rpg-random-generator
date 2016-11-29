package chars.random

object Generators {

  def next(bits: Int): Random[Int] = { seed =>
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    val next = (seed >>> (48 - bits))

    (newSeed, next.toInt)
  }

  def randomInt: Random[Int] = next(32)

}
