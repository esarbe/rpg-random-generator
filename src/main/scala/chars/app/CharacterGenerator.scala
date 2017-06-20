package chars.app

import chars.random.model.HumanoidGen
import chars.text.HumanoidDescriptionBuilder

object CharacterGenerator extends App {
  import chars.random.Random
  import chars.random.implicits._

  import cats.implicits._
  import cats._

  val humanoidStringPrinter = HumanoidDescriptionBuilder

  val seed = util.Random.nextLong
  val (_, desc) = HumanoidGen.map(humanoidStringPrinter.generate)(seed)

  println(s"($seed):\n $desc")
}
