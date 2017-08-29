package chars.random

import cats._
import cats.implicits._
import chars.random.Generators._
import enumeratum._

object implicits {

  implicit val randomMonad: Monad[Random] = new Monad[Random] {
    override def map[A, B](fa: Random[A])(f: (A) => B): Random[B] = {
      flatMap(fa)(a => pure(f(a)))
    }

    override def flatMap[A, B](fa: Random[A])(f: (A) => Random[B]): Random[B] = { seed =>
      val (nextSeed, value) = fa(seed)
      f(value)(nextSeed)
    }

    override def tailRecM[A, B](a: A)(f: (A) => Random[Either[A, B]]): Random[B] = defaultTailRecM(a)(f)

    override def pure[A](value: A): Random[A] = { seed =>
      (seed, value)
    }
  }

  def randomEnum[T <: EnumEntry](implicit ev: Enum[T]): Random[T] =
    for {
      rand <- randomInt
      index = Math.abs(rand) % ev.values.length
    } yield ev.values(index)


  def randomEnumWithWeights[T <: EnumEntry](toWeight: T => Double)(implicit ev: Enum[T]): Random[T] =
    randomValuesWithWeights(ev.values, toWeight)

  def randomValuesWithWeights[T](values: Seq[T], toWeight: T => Double): Random[T] = {
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
