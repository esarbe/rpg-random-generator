package chars.model.human.face.eye

import enumeratum.{Enum, EnumEntry}

sealed trait Color extends EnumEntry
object Color extends Enum[Color] {
  val values = findValues

  case object Brown extends Color
  case object Blue extends Color
  case object Hazel extends Color
  case object Green extends Color
  case object Grey extends Color
}
