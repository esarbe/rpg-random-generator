package chars.titfortat

import java.util.UUID

import io.estatico.newtype.macros.newtype

object Game {

  type Payoff = Double
  @newtype case class PlayerId(id: Int)
  case class Outcome(player: PlayerId, opponent: PlayerId, action: Action, payoff: Payoff)
  type Pairing = (Player, Player)
  type Player = (PlayerId, Strategy)

  trait Action

  object Action {
    case object Cooperate extends Action
    case object Defect extends Action
  }

  type InteractionEntryKey = (PlayerId, PlayerId)
  type InteractionEntry = (InteractionEntryKey, Action)
  implicit class HistoryOps(val history: Set[InteractionEntry]) extends AnyVal {
    def findLastAction(key: InteractionEntryKey): Option[InteractionEntry] = {
      history.toList.reverse.find(_._1 == key)
    }
  }

  case class State(score: Map[PlayerId, Payoff], mostRecentActions: Set[InteractionEntry])
  object State {
    def empty: State = State(Map.empty, Set.empty)
  }

  trait Strategy {
    def chose(state: State, self: Player, other: PlayerId): Action
  }


  type Payoffs = Map[(Action, Action), (Payoff, Payoff)]

  def evaluate(payoffs: Payoffs, state: State)(left: Player, right: Player): (Outcome, Outcome) = {

    def action(self: Player, other: PlayerId): Action = {
      self._2.chose(state, self, other)
    }

    val leftAction = action(left, right._1)
    val rightAction = action(right, left._1)

    val (leftPayoff, rightPayoff) = payoffs((leftAction, rightAction))

    (
      Outcome(left._1, right._1, rightAction, leftPayoff),
      Outcome(right._1, left._1, leftAction, rightPayoff)
    )
  }

  def runPairings(payoffs: Payoffs, state: State, pairings: Seq[Pairing]): State = {

    val outcomes =
      pairings
        .flatMap { case (left, right) =>
          val (lOutcome, rOutcome) = evaluate(payoffs, state)(left, right)
          List(lOutcome, rOutcome)
        }

    val newScore = outcomes.foldLeft(state.score) { case (acc: Map[PlayerId, Payoff], outcome) =>
      val oldScore = acc.getOrElse(outcome.player, 0.0)

      acc + (outcome.player -> (oldScore + outcome.payoff))
    }

    val mostRecentActions =
      outcomes.map { case Outcome(playerId, opponent, opponentAction, payoff) =>
        (playerId, opponent) -> opponentAction
      }.toSet

    state.copy(
      score = newScore,
      mostRecentActions = state.mostRecentActions ++ mostRecentActions
    )
  }

}
