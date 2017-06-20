package chars.text

import chars.model.Humanoid
import chars.model.Humanoid.{Age, Race, Sex}
import simulacrum.typeclass

object Texter

trait Description
case class Node(title: String, descriptions: Set[Description]) extends Description
case class Leaf(title: String, description: String) extends Description

@typeclass trait DescriptionBuilder[T] {
  def generate(t: T): Description
}

object DescriptionBuilders {
  implicit val ageToString: Age => String = {
    case Age.Young => "young"
    case Age.Ancient => "ancient"
    case Age.Middleaged => "middle-aged"
    case Age.Old => "old"
  }

  implicit val raceToString: Race => String = {
    case Race.Dwarf => "dwarf"
    case Race.Elf =>  "elf"
    case Race.Orc => "orc"
    case Race.Troll => "troll"
    case Race.Human => "human"
  }

  implicit val sexToString: Sex => String = {
    case Sex.Female => "female"
    case Sex.Male => "male"
    case Sex.Indeterminate => ""
  }
}

object HumanoidDescriptionBuilder extends DescriptionBuilder[Humanoid] {

  def buildDescription[T: DescriptionBuilder](t: T): Description = {
    implicitly[DescriptionBuilder[T]].generate(t)
  }

  def humanoidDescription(
     implicit
     ad: DescriptionBuilder[Age],
     rd: DescriptionBuilder[Race],
     sd: DescriptionBuilder[Sex]
  ): DescriptionBuilder[Humanoid] = new DescriptionBuilder[Humanoid] {
    override def generate(humanoid: Humanoid): Description = {
      import humanoid._

      val descriptions =
        Set(
          buildDescription(age),
          buildDescription(race),
          buildDescription(sex))

      Node(Humanoid.getClass.getSimpleName, descriptions)
    }
  }

  override def generate(t: Humanoid): Description = humanoidDescription.generate(t)
}
