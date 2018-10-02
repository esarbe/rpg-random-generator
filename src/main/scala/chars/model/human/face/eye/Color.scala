package chars.model.human.face.eye

import enumeratum.{Enum, EnumEntry}

sealed trait Color extends EnumEntry
object Color extends Enum[Color] {
  val values = findValues

  object Brown extends Color
  object Blue extends Color
  object Hazel extends Color
  object Green extends Color
}
