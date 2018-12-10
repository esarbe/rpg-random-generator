package chars.app

import cats.implicits._
import chars.cats.random.monad
import chars.random.Generator.oneOf
import chars.random._
import chars.titfortat.Game.Action.{Cooperate, Defect}
import chars.titfortat.Game.{Action, Payoffs, PlayerId, Score}
import chars.titfortat.IPD
import chars.titfortat.IPD._
import com.monovore.decline.{CommandApp, Opts}


//mvp:
// - (/) run IPD
// - (/) run IPD with participants type distribution taken from cmd line
// - run IPD interactively from cmd line
// - run IPD interactively from web gui
// - run IPD interactively concurrently
// - accounts
// - scoreboard
// - UX
// - context features
// - player traits

object TitForTat
  extends CommandApp(
    name = "ipd",
    header  = "Iterated Prisoners Dilemma",
    main =
  {

    val rounds = Opts.option[Int]("rounds", help = "rounds to play", short = "r")
    val numberDefect = Opts.option[Int]("defectors", help = "number of defectors", short = "d")
    val numberCooperators = Opts.option[Int]("cooperators", help = "number of cooperators", short = "c")
    val numberOfTitForTat = Opts.option[Int]("titfortat", help = "number of titfortat players", short = "t")
    val maybeSeed = Opts.option[Long]("seed", help = "random seed", "s").orNone

    val game = new TitForTat()

    (maybeSeed, rounds, numberDefect, numberCooperators, numberOfTitForTat).mapN {
      case (maybeSeed, rounds, numberDefect, numberCooperators, numberOfTitForTat) =>

        val seed = maybeSeed.getOrElse(0l)
        val distribution: Seq[(IPD.Strategy, Int)] =
          Seq(
            game.titForTat -> numberOfTitForTat,
            game.greedy -> numberDefect,
            game.naive -> numberCooperators)

        val endstate = game.runGame(distribution, rounds).run(seed)
        println(s"resulting state: ${endstate.mkString("\n")}")
    }
  }

)

class TitForTat() {

  import IPD._

  val titForTat = new Strategy {
    override def toString: String = "tft"
    override def chose(context: Context, player: PlayerImp, opponent: PlayerId): Action =
      context.getLastMove(opponent, player.id).getOrElse(Cooperate)
  }

  val greedy = new Strategy {
    override def toString: String = "greedy"
    override def chose(context: Context, player: PlayerImp, opponent: PlayerId): Action = Defect
  }

  val naive = new Strategy {
    override def toString: String = "naive"
    override def chose(context: Context, player: PlayerImp, opponent: PlayerId): Action = Cooperate
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

  def buildPairings(players: Seq[Player]): Random[Seq[Pairing]] = { seed: Long =>

    val ids = players

    ids.foldLeft((seed, Seq.empty[(Player, Player)])) { case ((seed, pairings), curr) =>

      val (newSeed, opponent) = oneOf(ids.filterNot(_ == curr):_*).apply(seed)

      (newSeed, pairings :+ (curr, opponent))
    }
  }

  def randomPairingRounds(rounds: Int)(players: Seq[Player]): Random[Seq[Pairings]] = {
    val randomPairingRounds = Seq.fill(rounds)(players).map(buildPairings)
    Generator.sequence(randomPairingRounds)
  }

  def runGame(distribution: Seq[(Strategy, Int)], rounds: Int): Random[Map[PlayerImp, Score]] = {
    randomPlayers(distribution)
      .flatMap(randomPairingRounds(rounds))
      .map { pairingRounds =>
        val participants = pairingRounds.flatten.toMap.keys

        val endState = pairingRounds.foldLeft(initialState) { case (state, pairings) =>
          pairings.foldLeft(state){ case (state, pairing) => runPairing(payoffs, state, pairing)}
        }

        (participants zip participants.map(_.id)).toMap.mapValues(endState.scores)
      }
  }
}
