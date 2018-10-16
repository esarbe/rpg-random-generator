package chars.model

import enumeratum.{Enum, EnumEntry}

sealed trait Culture extends EnumEntry
object Culture extends Enum[Culture] {

  val values = findValues
  case object German extends Culture
  case object USEnglish extends Culture
  case object Greek extends Culture
  case object Swahili extends Culture
  case object Chinese extends Culture
}