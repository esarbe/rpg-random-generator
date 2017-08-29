package chars.app

import chars.model.Humanoid
import chars.model.Humanoid._
import chars.random.model.HumanoidGen
import chars.text._

import scala.annotation.tailrec
import scala.collection.mutable

object CharacterGenerator extends App {
  import cats.implicits._
  import cats._

  import chars.random.Random
  import chars.random.implicits._

  import DescriptionBuilders._
  import HumanoidDescriptionBuilder._

  val sd: DescriptionBuilder[Sex] = buildDescriptionBuilder("sex")
  val ad: DescriptionBuilder[Age] = buildDescriptionBuilder("age")
  val rd: DescriptionBuilder[Race] = buildDescriptionBuilder("race")
  val hd: DescriptionBuilder[Humanoid] = humanoidDescriptionBuilder(ad, rd, sd)

  val seed = util.Random.nextLong
  val (_, desc) = HumanoidGen.map(hd.generate _)(seed)


  def pd(description: Description, indentation: Int = 0): String = description match {
    case Leaf(label, value) =>
      (1 to indentation).map(_ => '\t').mkString + s"$label: $value"
    case Node(label, descriptions) =>
      (1 to indentation).map(_ => '\t').mkString + s"l - $label:" +
      descriptions.map(pd(_, indentation + 1)).mkString("\n")
  }


  println(pd(desc))

  //println(s"($seed):\n $desc")
}
