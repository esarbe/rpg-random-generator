package chars.interaction

import enumeratum._

trait Interaction
object Interaction {

  trait ConfidenceTrick extends Interaction


  object ConfidenceGame {
    case object BrokenPorcelain extends ConfidenceTrick
  }


}


