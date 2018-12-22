package chars.titfortat

import cats.{Id, Monad}
import cats.implicits._
import chars.titfortat.PrisonersDilemma.Action.{Cooperate, Defect}
import chars.titfortat.PrisonersDilemma.{Action, Outcome, Payoffs, PlayerId, Score}
import io.estatico.newtype.macros.newtype

trait LastMoveMemory extends GameContextCapability {
  trait LastMoveContext extends ContextLike {
    def getLastMove(player: PlayerId, opponent: PlayerId): Option[Action]
  }
}

trait ScoreKnowledge extends GameContextCapability {
  trait ScoreContext extends ContextLike {
    def getScore(player: PlayerId): Score
  }
}

trait PayoffKnowledge extends GameContextCapability {
  trait PayoffContext extends ContextLike {
    def getPayoffs: Payoffs
  }
}

trait GameContextCapability {
  type Context <: ContextLike
  trait ContextLike
}

trait PrisonersDilemma[F[_]] extends GameContextCapability {
  type Pairings = Seq[Pairing]
  type Pairing = (Player, Player)
  type Player

  type State <: StateLike
  trait StateLike

  trait Strategy {
    def chose(context: Context, player: Player, opponent: PlayerId): F[Action]
  }

  val initialState: State
  def buildPlayer(id: PlayerId, strategy: Strategy): Player
  def buildContext(state: State, payoffs: Payoffs, player: PlayerId, opponent: PlayerId): Context
  def runPairing(payoffs: Payoffs, state: State, pairing: Pairing): F[State]
}




object PrisonersDilemma {
  type Score = Double
  type Payoff = Double
  @newtype case class PlayerId(id: Int)
  case class Outcome(player: PlayerId, opponent: PlayerId, action: Action, payoff: Payoff)

  sealed trait Action

  object Action {
    case object Cooperate extends Action
    case object Defect extends Action
  }

  type InteractionEntryKey = (PlayerId, PlayerId)
  type InteractionEntry = (InteractionEntryKey, Action)
  type Payoffs = Map[(Action, Action), (Payoff, Payoff)]
}