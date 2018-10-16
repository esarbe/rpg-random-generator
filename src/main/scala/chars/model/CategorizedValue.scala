package chars.model

import enumeratum.{Enum, EnumEntry}

sealed trait CategorizedValue extends EnumEntry
object CategorizedValue extends Enum[CategorizedValue] {
  val values = findValues
  case object Small extends CategorizedValue
  case object Medium extends CategorizedValue
  case object Large extends CategorizedValue
}