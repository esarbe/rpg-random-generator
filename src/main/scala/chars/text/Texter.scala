package chars.text

import chars.model.Humanoid
import chars.model.Humanoid.{Age, Race, Sex}
import simulacrum.typeclass

@typeclass trait StringPrinter[T] {
  def generate(t: T): String
}

object HumanoidStringPrinter extends StringPrinter[Humanoid] {

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

  implicit def printer[T](implicit f: T => String) = new StringPrinter[T] {
    def generate(t: T): String = f(t)
  }

  def humanoidDescription(
     implicit
     ad: StringPrinter[Age],
     rd: StringPrinter[Race],
     sd: StringPrinter[Sex]): StringPrinter[Humanoid] = new StringPrinter[Humanoid] {
    override def generate(humanoid: Humanoid): String = {
      import humanoid._

      s"""
         |A ${ad.generate(age)} ${sd.generate(sex)} ${rd.generate(race)}.
       """.stripMargin
    }
  }

  override def generate(t: Humanoid): String = humanoidDescription.generate(t)
}

