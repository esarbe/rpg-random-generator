package chars.model

import enumeratum.{Enum, EnumEntry}

sealed trait Culture extends EnumEntry
object Culture extends Enum[Culture] {
  val values = findValues
  object Earth extends Culture
  object Belt extends Culture
  object Mars extends Culture
}