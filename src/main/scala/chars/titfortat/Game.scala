package chars.titfortat

import chars.random.{Generator, Random}
import cats.implicits._
import chars.cats.random.monad
import io.estatico.newtype.macros.newtype

object Game {
  type Payoff = Double
  @newtype case class PlayerId(id: Double)
  case class Outcome(player: PlayerId, opponent: PlayerId, action: Action, payoff: Payoff)
  type Pairing = (Player, Player)
  type Player = (PlayerId, Strategy)

  trait Action

  object Action {
    case object Cooperate extends Action
    case object Defect extends Action
  }

  type InteractionEntryKey = (Set[PlayerId], PlayerId)
  type InteractionEntry = (InteractionEntryKey, Action)
  implicit class HistoryOps(val history: Set[InteractionEntry]) extends AnyVal {
    def findLastAction(key: InteractionEntryKey): Option[InteractionEntry] = {
      history.toList.reverse.find(_._1 == key)
    }
  }

  case class State(score: Map[PlayerId, Double], mostRecentActions: Set[InteractionEntry])

  trait Strategy {
    def chose(state: State, self: Player, other: PlayerId): Action
  }


  def randomPlayers(distribution: Map[Int, Strategy]): Random[Seq[Player]] = { seed: Long =>

    val newId = Generator.randomDouble.map(PlayerId)

    val strategies = distribution.flatMap { case (number, strategy) => List.fill(number)(strategy) }

    val init: (Long, Seq[(PlayerId, Strategy)]) = (seed, Seq.empty[(PlayerId, Strategy)])

    strategies.foldLeft(init){ case ((seed, participants), curr: Strategy) =>
      val (newSeed, id) = newId(seed)
      (newSeed, participants :+ (id, curr))
    }
  }


  type Payoffs = Map[(Action, Action), (Double, Double)]

  def evaluate(payoffs: Payoffs, state: State)(left: Player, right: Player): (Outcome, Outcome) = {

    def action(self: Player, other: PlayerId): Action = {
      self._2.chose(state, self, other)
    }

    val leftAction = action(left, right._1)
    val rightAction = action(right, left._1)

    val (leftPayoff, rightPayoff) = payoffs((leftAction, rightAction))

    (
      Outcome(left._1, right._1, leftAction, leftPayoff),
      Outcome(right._1, left._1, rightAction, rightPayoff)
    )
  }

  def runPairings(payoffs: Payoffs, state: State, pairings: Set[Pairing]): State = {

    val outcomes =
      pairings
        .toList
        .flatMap { case (left, right) =>
          val (lOutcome, rOutcome) = evaluate(payoffs, state)(left, right)
          List(lOutcome, rOutcome)
        }


    val newScore = outcomes.foldLeft(state.score) { case (acc, Outcome(playerId, opponent, action, payoff)) =>
      val oldScore = acc.getOrElse(playerId, 0)

      acc + (playerId -> (oldScore + payoff))
    }

    val mostRecentActions =
      outcomes.map { case Outcome(playerId, opponent, action, payoff) =>
        (Set(playerId, opponent), playerId) -> action
      }.toSet

    state.copy(
      score = newScore,
      mostRecentActions = mostRecentActions
    )
  }

}
