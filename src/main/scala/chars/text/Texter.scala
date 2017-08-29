package chars.text

import chars.model.Humanoid
import chars.model.Humanoid.{Age, Race, Sex}
import simulacrum.typeclass

trait Description
case class Node(label: String, descriptions: Seq[Description]) extends Description
case class Leaf(label: String, description: String) extends Description

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

object HumanoidDescriptionBuilder {

  def buildDescriptionBuilder[A](label: String)(implicit f: A => String): DescriptionBuilder[A] = new DescriptionBuilder[A]{
    override def generate(t: A): Description =Leaf(label, f(t))
  }

  private def buildDescription[T: DescriptionBuilder](t: T): Description = {
    implicitly[DescriptionBuilder[T]].generate(t)
  }

  def humanoidDescriptionBuilder(
     implicit
       ad: DescriptionBuilder[Age],
       rd: DescriptionBuilder[Race],
       sd: DescriptionBuilder[Sex]
  ): DescriptionBuilder[Humanoid] = new DescriptionBuilder[Humanoid] {
    override def generate(humanoid: Humanoid): Description = {
      import humanoid._

      val descriptions =
        Seq(
          buildDescription(age),
          buildDescription(race),
          buildDescription(sex))

      Node(Humanoid.getClass.getSimpleName, descriptions)
    }
  }
}
