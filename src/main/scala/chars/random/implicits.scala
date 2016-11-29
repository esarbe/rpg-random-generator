package chars.random

import cats.Monad
import chars.random.Generators.randomInt
import io.rbricks.itemized.{IndexedItemized, ItemizedIndex}

object implicits {

  implicit val randomMonad: Monad[Random] = new Monad[Random] {
    override def map[A, B](fa: Random[A])(f: (A) => B): Random[B] = {
      flatMap(fa)( a => pure(f(a)) )
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

  def randomEnum[T <: IndexedItemized](implicit ev: ItemizedIndex[T]): Random[T] =
    for {
      seed <- randomInt
      index = Math.abs(seed) % ev.indexMap.keys.size
    } yield ev.indexMap.values.toList(index)

}
