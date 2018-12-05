package chars.app

import java.util.UUID

import chars.cats.random.monad
import cats.implicits._
import chars.random.Generator.oneOf
import chars.titfortat.Game
import chars.titfortat.Game.Action.{Cooperate, Defect}
import chars.titfortat.Game._
import chars.random._

object TitForTat extends App {

  //mvp:
  // run IPD with participants type distribution taken from cmd line
  //

  val rounds = 100

  val titForTat = new Strategy {
    override def chose(state: State, self: (PlayerId, Strategy), other: PlayerId): Action =
      state.mostRecentActions
        .collectFirst { case (key, value) if key == (self._1, other.id) => value }
        .getOrElse(Cooperate)
    override def toString: String = "tft"
  }

  val greedy = new Strategy {
    override def chose(state: State, self: (PlayerId, Strategy), other: PlayerId): Action = Defect
    override def toString: String = "greedy"
  }

  val naive = new Strategy {
    override def chose(state: State, self: (PlayerId, Strategy), other: PlayerId): Action = Cooperate
    override def toString: String = "naive"
  }

  val distribution =
    Seq(
      titForTat -> 4,
      greedy -> 1,
      //naive -> 1
    )

  val payoffs: Payoffs = Map(
    (Cooperate, Cooperate) -> (3.0,3.0),
    (Cooperate, Defect) -> (0, 4),
    (Defect, Cooperate) -> (4, 0),
    (Defect, Defect) -> (1,1)
  )

  def randomPlayers(distribution: Seq[(Strategy, Int)]): Random[Seq[Player]] = { seed: Long =>

    val newId = Generator.randomInt.map(PlayerId.apply)

    val strategies = distribution.flatMap { case (strategy, number) => List.fill(number)(strategy) }

    val init = (seed, Seq.empty[(PlayerId, Strategy)])

    strategies.foldLeft(init){ case ((seed, participants), curr: Strategy) =>
      val (newSeed, id) = newId(seed)
      (newSeed, participants :+ (id, curr))
    }
  }

  def buildPairings(players: Seq[Player]): Random[Seq[(Player, Player)]] = { seed: Long =>

    val ids = players

    ids.foldLeft((seed, Seq.empty[(Player, Player)])) { case ((seed, pairings), curr) =>

      val (newSeed, opponent) = oneOf(ids.filterNot(_ == curr):_*).apply(seed)

      (newSeed, pairings :+ (curr, opponent))
    }
  }

  val pairings: Random[Seq[(Player, Player)]] = randomPlayers(distribution).flatMap(buildPairings)

  val randomPairingRounds: Random[Seq[Seq[Pairing]]] = randomPlayers(distribution).flatMap { players =>
    val randomPairingRounds = Seq.fill(rounds)(players).map(buildPairings)

    Generator.sequence(randomPairingRounds)
  }

  val result =
    randomPairingRounds.map { pairingRounds =>
      val state = State.empty
      val participants = pairingRounds.flatten.toMap.keys

      val endState = pairingRounds.foldLeft(state) { case (state, pairings) =>
        Game.runPairings(payoffs, state, pairings)
      }

      (participants zip participants.map(_._1)).toMap.mapValues(endState.score)
    }
  val endstate = result.get(1)

  println(s"resulting state: ${endstate.mkString("\n")}")

}
