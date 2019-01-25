package chars.titfortat

import cats.Monad
import cats.data.StateT
import cats.effect.IO
import cats.implicits._
import chars.app.ui
import chars.app.ui.{Console, Prompt, PromptConsoleInterpreter}
import chars.decline.random.Argument._
import chars.random._
import chars.titfortat.PrisonersDilemma.Action.{Cooperate, Defect}
import chars.titfortat.PrisonersDilemma.{Action, Payoffs, PlayerId, Score}
import chars.titfortat.service.PlayerInteraction
import com.monovore.decline.Opts

object TitForTat {

  type RandomIO[T] = StateT[IO, Seed, T]

  val rounds = Opts.option[Int]("rounds", help = "rounds to play", short = "r")
  val numberDefect = Opts.option[Int]("defectors", help = "number of defectors", short = "d")
  val numberCooperators = Opts.option[Int]("cooperators", help = "number of cooperators", short = "c")
  val numberOfTitForTat = Opts.option[Int]("titfortat", help = "number of titfortat players", short = "t")
  val maybeSeed = Opts.option[Seed]("seed", help = "random seed", "s").orNone
  val isInteractiveInput = Opts.flag("interactive", help = "run interactive mode", short = "i").orNone

  def buildGame(
    maybeSeed: Option[Seed],
    rounds: Int,
    numberDefectors: Int,
    numberCooperators: Int,
    numberOfTitForTat: Int,
    isInteractive: Option[Unit]
  )(implicit M: Monad[RandomIO]): Unit = {

    val prompt = new PromptConsoleInterpreter[RandomIO](new ui.ConsoleInterpreter[RandomIO])
    val ipd = new IteratedPrisonersDilemma[RandomIO]()

    val playerInteraction = new PlayerInteraction[RandomIO] {
      override def askForUserAction(playerId: PlayerId, context: IteratedPrisonersDilemma[RandomIO]#ContextImp): RandomIO[Action] = {
        lazy val input: RandomIO[Action] =
          prompt.prompt(
            s"""
               |
            | You play against player ${context.opponent}. This player's last move was to ${context.getLastMove(playerId, context.opponent)}
               | Your score is ${context.getScore(playerId)}. Your opponent's score is ${context.getScore(context.opponent)}
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

    val generator = new Generator[RandomIO] {
      override def next: StateT[IO, Seed, Long] = StateT { seed: Seed =>
        seed.next[IO].map { next =>
          (next, seed.value)
        }
      }
    }

    val game = new TitForTat(prompt, ipd, generator, playerInteraction)
    val seed = maybeSeed.getOrElse(Seed(0l))

    val distribution =
      Seq(
        game.game.titForTat -> numberOfTitForTat,
        game.game.defect -> numberDefectors,
        game.game.cooperate -> numberCooperators,
        game.interactive -> (if (isInteractive.isDefined) 1 else 0)
      )

    val endState =
      for {
        players <- game.randomPlayers(distribution)
        scores <- game.runGame(players, rounds)
      } yield {

        val playerById = players.toList.map(p => (p.id, p)).toMap

        val displayScores =
          scores
            .toList
            .sortBy(_._2)
            .reverse
            .map { case (id, score) => s"$id (${playerById(id).strategy})\t$score" }
            .mkString("\n")

        prompt.printLine(
          s"""
             | Final scores:
             | $displayScores
             |
           | ${ if (isInteractive.isDefined) "Thank you for playing." else "" }

            """.stripMargin)
      }

    endState.runA(seed).unsafeRunSync()
  }

  val runGame =
    (maybeSeed, rounds, numberDefect, numberCooperators, numberOfTitForTat, isInteractiveInput).mapN(buildGame)
}


class TitForTat[M[_]](
  prompt: Prompt[M] with Console[M],
  val game: IteratedPrisonersDilemma[M],
  generator: Generator[M],
  interaction: PlayerInteraction[M]
)(implicit M: Monad[M]) {

  import game._

  type Strategy = game.Strategy

  val interactive = new Strategy {
    override def chose(context: Context, player: Player, opponent: PlayerId): M[Action] =
      interaction.askForUserAction(player.id, context)
  }

  val payoffs: Payoffs = Map(
    (Cooperate, Cooperate) -> (3.0,3.0),
    (Cooperate, Defect) -> (0, 4),
    (Defect, Cooperate) -> (4, 0),
    (Defect, Defect) -> (1,1)
  )

  def buildPlayers(distribution: Seq[(Strategy, Int)]): M[Set[Player]] = {
    val strategies = distribution.flatMap { case (strategy, number) => List.fill(number)(strategy) }

    strategies
      .toList
      .traverse(i => generator.nextInt.map(PlayerId.apply).map(buildPlayer(_, i)))
      .map(_.toSet)
  }


  def buildPairings(players: Set[Player]): M[Seq[Pairing]] = {

    val ids = players.toList

    ids.traverse { id =>
        val opponent = generator.oneOf(ids.filterNot(_ == id):_*)
        opponent.map((id, _))
    }.map(_.toSeq)
  }


  def buildRoundsPairings(rounds: Int)(players: Set[Player]): M[Seq[Pairings]] =
    List.fill(rounds)(players).traverse(buildPairings).map(_.toSeq)


  def runGame(players: Set[Player], rounds: Int): M[Map[PlayerId, Score]] = {
    buildRoundsPairings(rounds)(players)
      .flatMap { pairingRounds =>
        val participants = pairingRounds.flatten.toMap.keys

        val endStateM = pairingRounds.foldLeft(M.pure(initialState)) { case (state, pairings) =>
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
