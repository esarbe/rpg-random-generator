package chars.model.human.face

import enumeratum.{Enum, EnumEntry}

sealed trait Shape extends EnumEntry
object Shape extends Enum[Shape] {
  val values = findValues
  object Round extends Shape
  object Oval extends Shape
  object Angular extends Shape
  object Square extends Shape
}