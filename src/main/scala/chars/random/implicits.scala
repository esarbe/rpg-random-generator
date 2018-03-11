package chars.random

import cats._
import cats.implicits._
import chars.random.Generators._
import enumeratum._

import scala.annotation.tailrec

object implicits {

  implicit val randomMonad: Monad[Random] = new Monad[Random] {

    override def flatMap[A, B](fa: Random[A])(f: (A) => Random[B]): Random[B] = { seed =>
      val (nextSeed, value) = fa(seed)
      f(value)(nextSeed)
    }

    override def pure[A](value: A): Random[A] = { seed =>
      (seed, value)
    }

    override def tailRecM[A, B](a: A)(f: A => Random[Either[A, B]]): Random[B] = {

      @tailrec
      def rec(a: A, f: A => Random[Either[A, B]], seed: Long): (Long, B) = {
        val (nextSeed, value) = f(a)(seed)
        value match {
          case Left(a) => rec(a, f, nextSeed)
          case Right(b) => (nextSeed, b)
        }
      }

      seed => rec(a, f, seed)
    }
  }
}
