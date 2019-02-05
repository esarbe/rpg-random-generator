package chars.model.human.face.eye

import enumeratum.{Enum, EnumEntry}

sealed trait Shape extends EnumEntry
object Shape extends Enum[Shape] {
  val values = findValues

  case object Oval extends Shape
  case object Round extends Shape
}