package chars.titfortat

import cats.Monad
import chars.titfortat.PrisonersDilemma.Action.{Cooperate, Defect}
import chars.titfortat.PrisonersDilemma._
import chars.titfortat.{LastMoveMemory, PayoffKnowledge, PrisonersDilemma, ScoreKnowledge}
import cats.implicits._

class IteratedPrisonersDilemma[M[_]](implicit M: Monad[M]) extends PrisonersDilemma[M] with LastMoveMemory with PayoffKnowledge with ScoreKnowledge {

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
    override def chose(context: Context, player: Player, opponent: PlayerId): M[Action] =
      implicitly[Monad[M]].pure(context.getLastMove(opponent, player.id).getOrElse(Cooperate))
  }

  val defect = new Strategy {
    override def toString: String = "greedy"
    override def chose(context: Context, player: Player, opponent: PlayerId): M[Action] = M.pure(Defect)
  }

  val cooperate = new Strategy {
    override def toString: String = "naive"
    override def chose(context: Context, player: Player, opponent: PlayerId): M[Action] = M.pure(Cooperate)
  }



  type Context = ContextImp
  case class ContextImp(state: State, payoffs: Payoffs) extends LastMoveContext with PayoffContext with ScoreContext {
    override def getLastMove(player: PlayerId, opponent: PlayerId): Option[Action] = state.history.get((player, opponent))
    override def getPayoffs: Payoffs = payoffs
    override def getScore(player: PlayerId): Score = state.scores.getOrElse(player, 0l)
  }

  override def buildPlayer(id: PlayerId, strategy: Strategy): PlayerImp =
    PlayerImp(id, strategy)

  def buildContext(state: State, payoffs: Payoffs, player: PlayerId, opponent: PlayerId): Context =
    ContextImp(state, payoffs)

  def runPairing(payoffs: Payoffs, state: State, pairing: Pairing): M[State] = {

    val (left, right) = pairing
    evaluate(payoffs, state)(left, right)
      .map { outcomes =>
        outcomes.foldLeft(state)(_ update _)
      }
  }

  def evaluate(payoffs: Payoffs, state: State)(left: Player, right: Player): M[Seq[Outcome]] = {

    def action(player: Player, other: PlayerId): M[Action] = {
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