package chars.model.human.face

import chars.model.human.face
import enumeratum.{Enum, EnumEntry}

sealed trait Eyes extends EnumEntry
object Eyes extends Enum[Eyes] {
  case class SameEyes(eye: face.Eye) extends Eyes
  case class DifferentEyes(left: face.Eye, right: face.Eye) extends Eyes

  val values = findValues
}
