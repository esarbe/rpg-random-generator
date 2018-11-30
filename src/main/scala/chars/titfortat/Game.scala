package chars.titfortat

import chars.random.Generator
import chars.cats.random._
import io.estatico.newtype.macros.newtype

object Game {
  @newtype case class PlayerId(id: Double)

  trait Action

  object Action {
    case object Cooperate extends Action
    case object Defect extends Action
  }

  case class State(score: Map[PlayerId, Double], mostRecentActions: Map[(Player, Player), Action])

  trait Player {
    def chose(opponentLastAction: Action): Action
  }

  trait Context

  val participants = {

    val newId = Generator.randomDouble.map(PlayerId)

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
