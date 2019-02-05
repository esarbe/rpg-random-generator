package chars.titfortat

import chars.titfortat.PrisonersDilemma.{Action, Payoffs, PlayerId}
import io.estatico.newtype.macros.newtype

trait GameContextCapability {
  type Context <: ContextLike
  trait ContextLike
}

trait PrisonersDilemma[F[_]] {
  self: GameContextCapability =>

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
