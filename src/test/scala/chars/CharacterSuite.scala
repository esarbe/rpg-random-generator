package chars

import cats.implicits._
import io.rbricks.itemized.{IndexedItemized, ItemizedIndex}
import io.rbricks.itemized.annotation.indexedEnum
import org.scalatest.FunSuite

class CharacterSuite extends FunSuite {

  import chars.random.Random
  import chars.random.implicits._
  import chars.random.Generators._

  def or[A](a: Random[A], b: Random[A]): Random[A] =
    for {
      f <- randomInt
      g <- if (f % 2 == 0) a
           else b
    } yield g

  def empty = randomMonad.pure(Nil)

  def many[A](r: Random[A]): Random[List[A]] = for {
    head <- r
    tail <- or(many(r), empty)
  } yield head :: tail






  test("random color") {
    println(many(randomPerson)(3))
  }

}
