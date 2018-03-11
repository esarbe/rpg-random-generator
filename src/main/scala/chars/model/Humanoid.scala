package chars.model

import chars.model.Humanoid.Body.Athleticism
import enumeratum._

sealed trait CategorizedValue extends EnumEntry
object CategorizedValue extends Enum[CategorizedValue] {
  val values = findValues
  object Small extends CategorizedValue
  object Medium extends CategorizedValue
  object Large extends CategorizedValue
}

trait Culture {
  trait ProfessionLike

}

trait Entity {

  case class Name(first: String)

}

object Humanoid extends Entity {

  sealed trait Profession extends EnumEntry
  object Profession extends Enum[Profession] {
    val values = findValues

  }

  sealed trait Race extends EnumEntry
  object Race extends Enum[Race] {
    val values = findValues
    object Human extends Race
    object Troll extends Race
    object Dwarf extends Race
    object Orc extends Race
    object Elf extends Race
  }

  sealed trait Sex extends EnumEntry
  object Sex extends Enum[Sex] {
    val values = findValues
    object Male extends Sex
    object Female extends Sex
    object Indeterminate extends Sex
  }

  sealed trait Age extends EnumEntry
  object Age extends Enum[Age] {
    val values = findValues
    object Young extends Age
    object Middleaged extends Age
    object Old extends Age
    object Ancient extends Age
  }

  object Body {
    case class Height(value: CategorizedValue)
    case class Weight(value: CategorizedValue)
    case class Athleticism(value: CategorizedValue)
  }

  case class Body(height: Body.Height, weight: Body.Weight, athleticism: Athleticism)

  case class Head(face: Face, size: CategorizedValue)
  case class Face(shape: Face.Shape, eyes: Face.Eyes.Eye)

  object Face {
    sealed trait Shape extends EnumEntry
    object Shape extends Enum[Shape] {
      val values = findValues
      object Round extends Shape
      object Oval extends Shape
      object Angular extends Shape
    }

    object Eyes {

      case class Eye(color: Eye.Color, shape: Eye.Shape)

      object Eye {
        sealed trait Shape extends EnumEntry
        object Shape extends Enum[Shape] {
          val values = findValues
          object Oval extends Shape
          object Round extends Shape
          object Sliteye extends Shape
        }

        sealed trait Color extends EnumEntry
        object Color extends Enum[Color] {
          val values = findValues
          object Brown extends Color
          object Blue extends Color
          object Hazel extends Color
          object Green extends Color
        }
      }
    }
  }
}
case class Humanoid(
  sex: Humanoid.Sex,
  age: Humanoid.Age,
  body: Humanoid.Body,
  head: Humanoid.Head
)
