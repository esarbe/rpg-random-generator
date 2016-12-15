package chars.random

object Generators {

  def next(bits: Int): Random[Int] = { seed =>
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    val next = (seed >>> (48 - bits))

    (newSeed, next.toInt)
  }

  def randomInt: Random[Int] = next(32)

  def randomDouble: Random[Double] = {
    import cats.implicits._
    import chars.random.implicits._

    (next(26) |@| next(27)).map {
      case (first, second) =>
        ((first << 27).toLong + second.toLong).toDouble * 1.1102230246251565E-16D
    }
  }

}
