package chars

import cats._
import io.rbricks.itemized.{IndexedItemized, Itemized, ItemizedCodec, ItemizedIndex}
import io.rbricks.itemized.annotation.{enum, indexedEnum}
import org.scalatest.FunSuite

class CharacterSuite extends FunSuite {

  type Random[T] = (Long) => (Long, T)

  implicit def randomMonad: Monad[Random] = new Monad[Random] {
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

  def next(bits: Int): Random[Int] = { seed =>
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    val next = (seed >>> (48 - bits))

    (newSeed, next.toInt)
  }

  def randomInt: Random[Int] = next(32)

  @indexedEnum trait Color {
    type Index = Int
    object Red { 0 }
    object Blue { 1 }
    object Yellow { 2 }
  }

  @indexedEnum trait Sex {
    type Index = Int
    object Male { 0 }
    object Female { 1 }
  }

  @indexedEnum trait Age {
    type Index = Int
    object Young { 0 }
    object Middleaged { 1 }
    object Old { 2 }
  }

  @indexedEnum trait ValueCategory {
    type Index = Int
    object Small { 0 }
    object Medium { 1 }
    object Large { 2 }
  }

  case class Height(value: ValueCategory)
  case class Weight(value: ValueCategory)

  object Face {
    @indexedEnum trait Shape {
      type Index = Int
      object Round { 0 }
      object Oval { 1 }
    }
  }

  case class Person(sex: Sex, age: Age, height: Height, weight: Weight )


  def enumGen[T <: IndexedItemized](implicit ev: ItemizedIndex[T]): Random[T] =
   for {
     seed <- randomInt
     index = seed % ev.indexMap.keys.size
  } yield ev.fromIndex(ev.indexMap.keys.toList(index)).get

  val randomColor = enumGen[Color]

  test("random color") {
    println(randomColor(1))
  }

}
