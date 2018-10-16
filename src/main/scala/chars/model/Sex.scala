package chars.model

import enumeratum.{Enum, EnumEntry}

sealed trait Sex extends EnumEntry
object Sex extends Enum[Sex] {
  val values = findValues

  case object Male extends Sex
  case object Female extends Sex
  case object Indeterminate extends Sex
}