package chars

import cats.Functor
import io.rbricks.itemized.{IndexedItemized, Itemized, ItemizedCodec, ItemizedIndex}
import io.rbricks.itemized.annotation.{enum, indexedEnum}
import org.scalatest.FunSuite


class CharacterSuite extends FunSuite {

  type Random[T] = (Long) => (Long, T)

  def next(bits: Int): Random[Int] = { seed =>
    val newSeed = (seed * 0x5DEECE66DL + 0xBL) & ((1L << 48) - 1)
    val next = (seed >>> (48 - bits))

    (newSeed, next.toInt)
  }

  def randomInt: Random[Int] = next(32)

  def always[T](value: T): Random[T] = { seed =>
    (seed, value)
  }

  implicit class RandomOps[A](run: Random[A]) {

    def map[B](f: A => B): Random[B] = {
      flatMap( a => always(f(a)) )
    }

    def flatMap[B](f: A => Random[B]): Random[B]= { seed =>
      val (nextSeed, value) = run(seed)
      f(value)(nextSeed)
    }
  }

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
