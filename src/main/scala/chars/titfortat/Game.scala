package chars.titfortat

import io.estatico.newtype.macros.newtype



object Game {
  @newtype case class PlayerId(id: Double)

  trait Action

  object Action {
    case object Cooperate extends Action
    case object Defect extends Action
  }

  case class State(lastActions: Map[(Player, Player), Action])


  trait Player {
    def chose(opponentLastAction: Action): Action
  }

  trait Context

  val participants = {
    val titForTat = new Player {
      override def chose(opponentLastAction: Action): Action = opponentLastAction
    }

    val greedy = new Player {
      override def chose(opponentLastAction: Action): Action = Action.Defect
    }

    val naive = new Player {
      override def chose(opponentLastAction: Action): Action = Action.Cooperate
    }

    val distribution =
      Map(
        titForTat -> 10,
        greedy -> 20,
        naive -> 100
      )

    distribution.map { case (generator) }
  }




  val participants =

}
