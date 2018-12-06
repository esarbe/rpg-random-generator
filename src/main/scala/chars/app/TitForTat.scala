package chars.app

import java.util.UUID

import chars.cats.random.monad
import cats.implicits._
import chars.random.Generator.oneOf
import chars.titfortat.{Game, IPD}
import chars.titfortat.Game.Action.{Cooperate, Defect}
import chars.titfortat.Game._
import chars.random._

object TitForTat extends App {

  import IPD._

  //mvp:
  // run IPD with participants type distribution taken from cmd line
  //

  val rounds = 100

  val titForTat = new Strategy {
    override def toString: String = "tft"
    override def chose(context: Context, player: PlayerImp, opponent: PlayerId): Action =
      context.getLastMove(opponent, player.id).getOrElse(Cooperate)
  }

  val greedy = new Strategy {
    override def toString: String = "greedy"
    override def chose(context: ContextImp, player: PlayerImp, opponent: PlayerId): Action = Defect
  }

  val naive = new Strategy {
    override def toString: String = "naive"
    override def chose(context: ContextImp, player: PlayerImp, opponent: PlayerId): Action = Cooperate
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

    val init = (seed, Seq.empty[Player])

    strategies.foldLeft(init){ case ((seed, participants), curr: Strategy) =>
      val (newSeed, id) = newId(seed)
      (newSeed, participants :+ buildPlayer(id, curr))
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
      val participants = pairingRounds.flatten.toMap.keys

      val endState = pairingRounds.foldLeft(initialState) { case (state, pairings) =>
        pairings.foldLeft(state){ case (state, pairing) => runPairing(payoffs, state, pairing)}
      }

      (participants zip participants.map(_.id)).toMap.mapValues(endState.scores)
    }
  val endstate = result.get(1)

  println(s"resulting state: ${endstate.mkString("\n")}")

}
