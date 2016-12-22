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


    (next(26) |@| next(27)).map { case (a, b) =>
      ((a.toLong << 27) + b) / (1.toLong << 53).toDouble
    }
  }

  def chooseDouble(h: Double, l: Double): Random[Double] = {
    import cats.implicits._
    import chars.random.implicits._

    randomDouble.map { x =>
      val (ll, hh) = if(h < l) (h, l) else (l, h)
      val diff = hh - ll
      ll + x * diff
    }
  }
}
