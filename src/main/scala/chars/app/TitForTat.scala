package chars.app

import cats.Monad
import cats.data.State
import cats.effect.IO
import cats.implicits._
import chars.decline.random.Argument._
import chars.app.ui.{Console, Prompt, PromptConsoleInterpreter}
import chars.random.Generator.oneOf
import chars.random._
import chars.titfortat.Game.Action.{Cooperate, Defect}
import chars.titfortat.Game.{Action, Payoffs, PlayerId, Score}
import chars.titfortat.IPD
import com.monovore.decline.{CommandApp, Opts}


//mvp:
// - (/) run IPD
// - (/) run IPD with participants type distribution taken from cmd line
// - (/) run IPD interactively from cmd line
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
    val maybeSeed = Opts.option[Seed]("seed", help = "random seed", "s").orNone
    val isInteractiveInput = Opts.flag("interactive", help = "run interactive mode", short = "i").orNone

    (maybeSeed, rounds, numberDefect, numberCooperators, numberOfTitForTat, isInteractiveInput).mapN {
      case (maybeSeed, rounds, numberDefect, numberCooperators, numberOfTitForTat, isInteractive) =>

        val prompt = new PromptConsoleInterpreter[IO](ui.ConsoleInterpreter)
        val game = new TitForTat(prompt)
        val seed = maybeSeed.getOrElse(Seed(0l))
        val distribution: Seq[(game.Strategy, Int)] =
          Seq(
            game.game.titForTat -> numberOfTitForTat,
            game.game.greedy -> numberDefect,
            game.game.naive -> numberCooperators,
            game.interactive -> (if (isInteractive.isDefined) 1 else 0))


        game.runGame(distribution, rounds).run(seed).value._2.flatMap { scores =>

          val displayScores =
            scores
              .toList
              .sortBy(_._2)
              .reverse
              .map { case (id, score) => s"$id\t$score" }
              .mkString("\n")

          prompt.printLine(
            s"""
              | Final scores:
              | $displayScores
              |
              | ${ if (isInteractive.isDefined) "Thank you for playing." else "" }

            """.stripMargin)
        }.unsafeRunSync()


    }
  }

)

class TitForTat[F[_]: Monad](prompt: Prompt[F] with Console[F]) {

  val game = new IPD[F]

  import game.{Context, Player, Pairing, Pairings, initialState, runPairing, buildPlayer, greedy, titForTat}

  type Strategy = game.Strategy

  def pure[A](a: A) = implicitly[Monad[F]].pure(a)

  val interactive = new Strategy {
    override def chose(context: Context, player: Player, opponent: PlayerId): F[Action] = {
      lazy val input: F[Action] =
        prompt.prompt(
          s"""
            |
            | You play against player $opponent. This player's last move was to ${context.getLastMove(player.id, opponent)}
            | Your score is ${context.getScore(player.id)}. Your opponent's score is ${context.getScore(opponent)}
            | What do you chose to do? (c)ooperate or (d)efect?
          """.stripMargin
        ).flatMap {
          case "d" => pure(Defect)
          case "c" => pure(Cooperate)
          case _ => input
        }

      input
    }
  }


  val distribution =
    Seq(
      titForTat -> 4,
      greedy -> 1,
      interactive -> 1
    )

  val payoffs: Payoffs = Map(
    (Cooperate, Cooperate) -> (3.0,3.0),
    (Cooperate, Defect) -> (0, 4),
    (Defect, Cooperate) -> (4, 0),
    (Defect, Defect) -> (1,1)
  )

  def randomPlayers(distribution: Seq[(Strategy, Int)]): Random[Seq[Player]] = State { seed =>

    val newId = Generator.randomInt.map(PlayerId.apply)

    val strategies = distribution.flatMap { case (strategy, number) => List.fill(number)(strategy) }

    val init = (seed, Seq.empty[Player])

    strategies.foldLeft(init){ case ((seed, participants), curr: Strategy) =>
      val (newSeed, id) = newId.run(seed).value
      (newSeed, participants :+ buildPlayer(id, curr))
    }
  }

  def buildPairings(players: Seq[Player]): Random[Seq[Pairing]] = State { seed: Seed =>

    val ids = players

    ids.foldLeft((seed, Seq.empty[(Player, Player)])) { case ((seed, pairings), curr) =>

      val (newSeed, opponent) = oneOf(ids.filterNot(_ == curr):_*).run(seed).value

      (newSeed, pairings :+ (curr, opponent))
    }
  }

  def randomPairingRounds(rounds: Int)(players: Seq[Player]): Random[Seq[Pairings]] = {
    val randomPairingRounds = Seq.fill(rounds)(players).map(buildPairings)
    Generator.sequence(randomPairingRounds)
  }

  def runGame(distribution: Seq[(Strategy, Int)], rounds: Int): Random[F[Map[PlayerId, Score]]] = {
    randomPlayers(distribution)
      .flatMap(randomPairingRounds(rounds))
      .map { pairingRounds =>
        val participants = pairingRounds.flatten.toMap.keys

        val endStateM = pairingRounds.foldLeft(pure(initialState)) { case (state, pairings) =>
          pairings.foldLeft(state){ case (stateM, pairing) => stateM.flatMap(runPairing(payoffs, _, pairing)) }
        }

        endStateM.map { endstate =>

          participants.map { participant =>
            participant.id -> endstate.scores(participant.id)
          }.toMap
        }
      }
    }
}
