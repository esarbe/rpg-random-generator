package chars.interaction

import enumeratum._

sealed trait Society extends EnumEntry
object Society extends Enum[Society] {
  val values = findValues
  case object Religious extends Society
  case object Political extends Society
  case object Cultural extends Society
}
