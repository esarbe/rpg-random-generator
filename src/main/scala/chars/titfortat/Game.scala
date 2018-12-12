package chars.titfortat

import cats.{Id, Monad}
import cats.implicits._
import chars.titfortat.Game.Action.{Cooperate, Defect}
import chars.titfortat.Game.{Action, Outcome, Payoffs, PlayerId, Score}
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

trait Game[F[_]] extends GameContextCapability {
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



class IPD[F[_]: Monad] extends Game[F] with LastMoveMemory with PayoffKnowledge with ScoreKnowledge {

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

  val titForTat = new Strategy {
    override def toString: String = "tft"
    override def chose(context: Context, player: Player, opponent: PlayerId): F[Action] =
      implicitly[Monad[F]].pure(context.getLastMove(opponent, player.id).getOrElse(Cooperate))
  }

  val greedy = new Strategy {
    override def toString: String = "greedy"
    override def chose(context: Context, player: Player, opponent: PlayerId): F[Action] = pure(Defect)
  }

  val naive = new Strategy {
    override def toString: String = "naive"
    override def chose(context: Context, player: Player, opponent: PlayerId): F[Action] = pure(Cooperate)
  }



  type Context = ContextImp
  case class ContextImp(state: State, payoffs: Payoffs) extends LastMoveContext with PayoffContext with ScoreContext {
    override def getLastMove(player: PlayerId, opponent: PlayerId): Option[Action] = state.history.get((player, opponent))
    override def getPayoffs: Payoffs = payoffs
    override def getScore(player: PlayerId): Score = state.scores.getOrElse(player, 0l)
  }

  def pure[A](a: A): F[A] = implicitly[Monad[F]].pure(a)

  override def buildPlayer(id: PlayerId, strategy: Strategy): PlayerImp =
    PlayerImp(id, strategy)

  def buildContext(state: State, payoffs: Payoffs, player: PlayerId, opponent: PlayerId): Context =
    ContextImp(state, payoffs)

  def runPairing(payoffs: Payoffs, state: State, pairing: Pairing): F[State] = {

    val (left, right) = pairing
    evaluate(payoffs, state)(left, right)
      .map { outcomes =>
        outcomes.foldLeft(state)(_ update _)
      }
  }

  def evaluate(payoffs: Payoffs, state: State)(left: Player, right: Player): F[Seq[Outcome]] = {

    def action(player: Player, other: PlayerId): F[Action] = {
      val context = buildContext(state, payoffs, player.id, other)
      player.strategy.chose(context, player, other)
    }

    for {
      leftAction <- action(left, right.id)
      rightAction <- action(right, left.id)
    } yield {
      val (leftPayoff, rightPayoff) = payoffs((leftAction, rightAction))

      Seq(
        Outcome(left.id, right.id, leftAction, leftPayoff),
        Outcome(right.id, left.id, rightAction, rightPayoff)
      )
    }
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
