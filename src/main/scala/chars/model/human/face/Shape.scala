package chars.model.human.face

import enumeratum.{Enum, EnumEntry}

sealed trait Shape extends EnumEntry
object Shape extends Enum[Shape] {
  val values = findValues
  case object Round extends Shape
  case object Oval extends Shape
  case object Angular extends Shape
  case object Square extends Shape
}