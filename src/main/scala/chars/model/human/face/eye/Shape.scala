package chars.model.human.face.eye

import enumeratum.{Enum, EnumEntry}

sealed trait Shape extends EnumEntry
object Shape extends Enum[Shape] {
  val values = findValues
  object Oval extends Shape
  object Round extends Shape
}