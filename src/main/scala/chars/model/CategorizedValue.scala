package chars.model

import enumeratum.{Enum, EnumEntry}

sealed trait CategorizedValue extends EnumEntry
object CategorizedValue extends Enum[CategorizedValue] {
  val values = findValues
  object Small extends CategorizedValue
  object Medium extends CategorizedValue
  object Large extends CategorizedValue
}