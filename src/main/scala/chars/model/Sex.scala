package chars.model

import enumeratum.{Enum, EnumEntry}

sealed trait Sex extends EnumEntry
object Sex extends Enum[Sex] {
  val values = findValues
  object Male extends Sex
  object Female extends Sex
  object Indeterminate extends Sex
}