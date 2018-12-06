package chars.titfortat

import chars.titfortat.Game.{Action, Outcome, Payoffs, PlayerId, Score}
import io.estatico.newtype.macros.newtype

trait LastMoveMemory extends Game {
  trait LastMoveContext extends ContextLike {
    def getLastMove(player: PlayerId, opponent: PlayerId): Option[Action]
  }
}

trait ScoreKnowledge extends Game {
  trait ScoreContext extends ContextLike {
    def getScore(player: PlayerId): Score
  }
}

trait PayoffKnowledge extends Game {
  trait PayoffContext extends ContextLike {
    def getPayoffs: Payoffs
  }
}

trait Game {
  type Pairing = (Player, Player)
  type Player

  type State <: StateLike
  trait StateLike

  type Context <: ContextLike
  trait ContextLike

  trait Strategy {
    def chose(context: Context, player: Player, opponent: PlayerId): Action
  }
  val initialState: State
  def buildPlayer(id: PlayerId, strategy: Strategy): Player
  def buildContext(state: State, player: PlayerId, opponent: PlayerId): Context
  def runPairing(payoffs: Payoffs, state: State, pairing: Pairing): State
}


object IPD extends Game with LastMoveMemory with PayoffKnowledge with ScoreKnowledge {

  type Player = PlayerImp
  case class PlayerImp(id: PlayerId, strategy: Strategy)


  type State = StateImp
  case class StateImp(history: Map[(PlayerId, PlayerId), Action], scores: Map[PlayerId, Score]) extends StateLike {
    def update(outcome: Outcome): StateImp = copy(
      history = history + ((outcome.player, outcome.opponent) -> outcome.action),
      scores = scores + (outcome.player -> (scores.getOrElse(outcome.player, 0.0) + outcome.payoff))
    )
  }

  val initialState = StateImp(Map.empty[(PlayerId, PlayerId), Action], Map.empty[PlayerId, Score])


  type Context = ContextImp
  case class ContextImp(state: State) extends LastMoveContext with PayoffContext with ScoreContext {
    override def getLastMove(player: PlayerId, opponent: PlayerId): Option[Action] = state.history.get((player, opponent))
    override def getPayoffs: Payoffs = ???
    override def getScore(player: PlayerId): Score = ???
  }

  override def buildPlayer(id: PlayerId, strategy: IPD.Strategy): PlayerImp = PlayerImp(id, strategy)

  def buildContext(state: State, player: PlayerId, opponent: PlayerId): ContextImp = ContextImp(state)
  def runPairing(payoffs: Payoffs, state: State, pairing: Pairing): State = {

    val (left, right) = pairing
    val outcomes = evaluate(payoffs, state)(left, right)

    outcomes.foldLeft(state)(_ update _)
  }

  def evaluate(payoffs: Payoffs, state: State)(left: Player, right: Player): Seq[Outcome] = {

    def action(player: Player, other: PlayerId): Action = {
      player.strategy.chose(buildContext(state, player.id, other), player, other)
    }

    val leftAction = action(left, right.id)
    val rightAction = action(right, left.id)

    val (leftPayoff, rightPayoff) = payoffs((leftAction, rightAction))

    Seq(
      Outcome(left.id, right.id, leftAction, leftPayoff),
      Outcome(right.id, left.id, rightAction, rightPayoff)
    )
  }


}

object Game {
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
