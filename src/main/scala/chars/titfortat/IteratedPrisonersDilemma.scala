package chars.titfortat

import cats.Applicative
import chars.titfortat.PrisonersDilemma.Action.{Cooperate, Defect}
import chars.titfortat.PrisonersDilemma._
import cats.implicits._

class IteratedPrisonersDilemma[A[_]](implicit A: Applicative[A])
  extends PrisonersDilemma[A]
     with LastMoveMemory
     with PayoffKnowledge
     with ScoreKnowledge
     with OpponentKnowledge {

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
    override def chose(context: Context, player: Player, opponent: PlayerId): A[Action] =
      A.pure(context.getLastMove(opponent, player.id).getOrElse(Cooperate))
  }

  val defect = new Strategy {
    override def toString: String = "defect"
    override def chose(context: Context, player: Player, opponent: PlayerId): A[Action] = A.pure(Defect)
  }

  val cooperate = new Strategy {
    override def toString: String = "cooperate"
    override def chose(context: Context, player: Player, opponent: PlayerId): A[Action] = A.pure(Cooperate)
  }


  type Context = ContextImp
  case class ContextImp(state: State, opponent: PlayerId, payoffs: Payoffs)
      extends LastMoveContext
      with PayoffContext
      with ScoreContext
      with OpponentContext {
    override def getLastMove(player: PlayerId, opponent: PlayerId): Option[Action] = state.history.get((player, opponent))
    override def getPayoffs: Payoffs = payoffs
    override def getScore(player: PlayerId): Score = state.scores.getOrElse(player, 0l)
    override def getOpponent: PlayerId = opponent
  }

  override def buildPlayer(id: PlayerId, strategy: Strategy): PlayerImp =
    PlayerImp(id, strategy)

  def buildContext(state: State, payoffs: Payoffs, player: PlayerId, opponent: PlayerId): Context =
    ContextImp(state, opponent, payoffs)

  def runPairing(payoffs: Payoffs, state: State, pairing: Pairing): A[State] = {

    val (left, right) = pairing
    evaluate(payoffs, state)(left, right)
      .map { outcomes =>
        outcomes.foldLeft(state)(_ update _)
      }
  }

  def evaluate(payoffs: Payoffs, state: State)(left: Player, right: Player): A[Seq[Outcome]] = {
    def action(player: Player, other: PlayerId): A[Action] = {
      val context = buildContext(state, payoffs, player.id, other)
      player.strategy.chose(context, player, other)
    }

    (action(left, right.id), action(right, left.id)).mapN {
      (leftAction, rightAction) =>
        val (leftPayoff, rightPayoff) = payoffs((leftAction, rightAction))

        Seq(
          Outcome(left.id, right.id, leftAction, leftPayoff),
          Outcome(right.id, left.id, rightAction, rightPayoff)
        )
    }
  }
}

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

trait OpponentKnowledge extends GameContextCapability {
  trait OpponentContext extends ContextLike {
    def getOpponent: PlayerId
  }
}

trait PayoffKnowledge extends GameContextCapability {
  trait PayoffContext extends ContextLike {
    def getPayoffs: Payoffs
  }
}
