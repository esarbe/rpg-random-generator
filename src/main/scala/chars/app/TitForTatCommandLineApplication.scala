package chars.app

import cats._
import cats.data.StateT
import cats.effect.IO
import cats.implicits._
import chars.app.ui.PromptConsoleInterpreter
import chars.decline.random.argument._
import chars.random.{Generator, Seed}
import chars.titfortat.PrisonersDilemma.Action.{Cooperate, Defect}
import chars.titfortat.PrisonersDilemma.{Action, PlayerId}
import chars.titfortat.service.PlayerInteraction
import chars.titfortat.{IteratedPrisonersDilemma, TitForTat}
import com.monovore.decline.{CommandApp, Opts}


object TitForTatCommandLineApplication
  extends CommandApp(
    name = "TitForTat",
    header = "Iterated Prisoner's Dilemma with Extra Sauce",
    main = TitForTatCommandLine.runGame,
    helpFlag = true,
    version = "0.0.1-SNAPSHOT")


object TitForTatCommandLine {

  type RandomIO[T] = StateT[IO, Seed, T]

  val rounds = Opts.option[Int]("rounds", help = "rounds to play", short = "r")
  val numberDefect = Opts.option[Int]("defectors", help = "number of defectors", short = "d")
  val numberCooperators = Opts.option[Int]("cooperators", help = "number of cooperators", short = "c")
  val numberOfTitForTat = Opts.option[Int]("titfortat", help = "number of titfortat players", short = "t")
  val maybeSeed = Opts.option[Seed]("seed", help = "random seed", "s").orNone
  val isInteractiveInput = Opts.flag("interactive", help = "run interactive mode", short = "i").orNone

  val prompt = new PromptConsoleInterpreter[RandomIO](new ui.ConsoleInterpreter[RandomIO])
  val ipd = new IteratedPrisonersDilemma[RandomIO]()

  val generator = new Generator[RandomIO] {
    override def next: StateT[IO, Seed, Long] = StateT { seed: Seed =>
      seed.next[IO].map { next =>
        (next, seed.value)
      }
    }
  }


  def buildGame[M[_]](
    prompt: PromptConsoleInterpreter[M],
    ipd: IteratedPrisonersDilemma[M],
    generator: Generator[M]
  )(
    rounds: Int,
    numberDefectors: Int,
    numberCooperators: Int,
    numberOfTitForTat: Int,
    isInteractive: Option[Unit])(
    implicit M: Monad[M]
  ): M[Unit] = {

    val playerInteraction = new PlayerInteraction[M] {
      override def askForUserAction(
        playerId: PlayerId,
        context: IteratedPrisonersDilemma[M]#Context
      ): M[Action] = {
        val opponentsLastMove = context.getLastMove(playerId, context.opponent)
        val opponentsScore = context.getScore(context.opponent)
        lazy val input: M[Action] =
          prompt.prompt(
            s"""
               | You play against player ${context.opponent}. This player's last move was to $opponentsLastMove
               | Your score is ${context.getScore(playerId)}. Your opponent's score is $opponentsScore
               | What do you chose to do? (c)ooperate or (d)efect?
          """.stripMargin
          ).flatMap {
            case "d" => M.pure(Defect)
            case "c" => M.pure(Cooperate)
            case _ => input
          }

        input
      }
    }

    val game = new TitForTat(ipd, generator, playerInteraction)

    val distribution =
      Seq(
        game.game.titForTat -> numberOfTitForTat,
        game.game.defect -> numberDefectors,
        game.game.cooperate -> numberCooperators,
        game.interactive -> (if (isInteractive.isDefined) 1 else 0)
      )

    val endState: M[Unit] =
      for {
        //_ <- prompt.printLine(s"Seed is $seed")
        players <- game.buildPlayers(distribution)
        scores <- game.runGame(players, rounds)


        playerById = players.toList.map(p => (p.id, p)).toMap

        displayScores =
          scores
            .toList
            .sortBy(_._2)
            .reverse
            .map { case (id, score) => s"$id (${playerById(id).strategy})\t$score" }
            .mkString("\n")

        endState <- prompt.printLine(
          s"""
             | Final scores:
             | $displayScores
             |
           | ${ if (isInteractive.isDefined) "Thank you for playing." else "" }

            """.stripMargin)
      } yield endState
    endState
  }

  val game =
    (rounds, numberDefect, numberCooperators, numberOfTitForTat, isInteractiveInput)
    .mapN(buildGame(prompt, ipd, generator))

  val runGame = (maybeSeed, game).mapN { (seed, game) =>
    game.runA(seed.getOrElse(Seed(0))).unsafeRunSync()
  }


}

