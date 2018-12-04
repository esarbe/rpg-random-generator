package chars.app

import chars.random.{Generator, Random}
import chars.cats.random.monad
import cats.implicits._
import chars.random.Generator.oneOf
import chars.titfortat.Game
import chars.titfortat.Game._

object TitForTat extends {
  //mvp:
  // run IPD with participants type distribution taken from cmd line
  //

  val titForTat = new Strategy {
    def chose(opponentLastAction: Action): Action = opponentLastAction
  }

  val greedy = new Strategy {
    def chose(opponentLastAction: Action): Action = Action.Defect
  }

  val naive = new Strategy {
    def chose(opponentLastAction: Action): Action = Action.Cooperate
  }

  val distribution =
    Map(
      10 -> titForTat,
      20 -> greedy,
      100 -> naive
    )

  val randomPlayers: Random[Seq[Player]] = Game.randomPlayers(distribution)

  val pairings = randomPlayers.flatMap(buildPairings)

  def buildPairings(players: Seq[Player]): Random[Seq[(PlayerId, PlayerId)]] = { seed: Long =>

    val ids = players.map(_._1)

    ids.foldLeft((seed, Seq.empty[(PlayerId, PlayerId)])) { case ((seed, pairings), curr) =>

      val (newSeed, opponent) = oneOf(ids.filterNot(_ == curr):_*).apply(seed)

      (newSeed, pairings :+ (curr, opponent))
    }
  }

  val state =
    State(
      score = Map.empty,
      mostRecentActions = Map.empty
    )




}
