package chars.random

object Generators {

  import cats.implicits._
  import chars.random.implicits._

  def next(bits: Int): Random[Int] = { seed =>
    val newSeed = seed * 25214903917L + 11L & 281474976710655L
    val next = newSeed >>> 48 - bits

    (newSeed, next.toInt)
  }

  def randomInt: Random[Int] = next(32)

  val randomLong: Random[Long] = for {
    a <- next(32)
    b <- next(32)
  } yield (a.toLong << 32) + b.toLong


  val randomDouble = for {
    a <- next(26)
    b <- next(27)
  } yield {
    ((a.toLong << 27) + b.toLong).toDouble / (1L << 53).toDouble
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
