package chars.random

import cats.{Id, Monad}
import cats.data.State
import cats.implicits._
import enumeratum.{Enum, EnumEntry}

trait Generator[F[_]] {
  def next: F[Long]
}

object RandomGenerator extends Generator[Random] {
  override def next: Random[Long] = State { seed =>
    (seed.next[Id], seed.value)
  }
}

object Generator {

  implicit class GeneratorOps[F[_]: Monad](generator: Generator[F]) {
    def next(bits: Int): F[Int] = generator.next.map(long => (long >>> 48 - bits).toInt)

    def randomInt: F[Int] = next(32)

    val randomLong: F[Long] = for {
      a <- next(32)
      b <- next(32)
    } yield (a.toLong << 32) + b.toLong


    val randomDouble = for {
      a <- next(26)
      b <- next(27)
    } yield {
      ((a.toLong << 27) + b.toLong).toDouble / (1L << 53).toDouble
    }

    def chooseDouble(low: Double, high: Double): F[Double] = {
      randomDouble.map { x =>
        val (ll, hh) = if (high < low) (high, low) else (low, high)
        val diff = hh - ll
        ll + x * diff
      }
    }

    def oneOf[T](values: T*): F[T] =
      for {
        rand <- randomInt
        index = Math.abs(rand) % values.length
      } yield values.apply(index)

    def randomEnum[T <: EnumEntry](implicit ev: Enum[T]): F[T] =
      oneOf(ev.values:_*)

    def randomEnumWithWeights[T <: EnumEntry](
      toWeight: T => Double
    )(implicit ev: Enum[T]): F[T] =
      randomValuesWithWeights(ev.values, toWeight)


    def constant[T](value: T): F[T] =
      for (_ <- randomLong) yield value

    def randomValuesWithWeights[T](
      values: Seq[T], toWeight: T => Double
    ): F[T] = {

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
}

