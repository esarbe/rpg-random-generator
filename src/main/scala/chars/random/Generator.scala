package chars.random

import enumeratum.{Enum, EnumEntry}

object Generator {

  import chars.random.CatsInstances._
  import cats.implicits._

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

  def chooseDouble(low: Double, high: Double): Random[Double] = {
    randomDouble.map { x =>
      val (ll, hh) = if(high < low) (high, low) else (low, high)
      val diff = hh - ll
      ll + x * diff
    }
  }


  def oneOf[T](values: T*): Random[T] =
    for {
      rand <- randomInt
      index = Math.abs(rand) % values.length
    } yield values.apply(index)


  def randomEnum[T <: EnumEntry](implicit ev: Enum[T]): Random[T] =
    oneOf(ev.values:_*)


  def randomEnumWithWeights[T <: EnumEntry](
      toWeight: T => Double
  )(implicit ev: Enum[T]): Random[T] =
    randomValuesWithWeights(ev.values, toWeight)


  def constant[T](value: T): Random[T] =
    for (_ <- randomLong) yield value

  def randomValuesWithWeights[T](
      values: Seq[T], toWeight: T => Double
  ): Random[T] = {

    val weights = values.map(toWeight)
    val sum = weights.sum
    val normalizedWeights = weights.map(_ / sum)

    for {
      limit <- randomDouble
    } yield {
      var index = -1
      var sum = 0.0
      while (sum < limit) {
        index += 1
        sum = sum + normalizedWeights(index)
      }

      values(index)
    }
  }
}
