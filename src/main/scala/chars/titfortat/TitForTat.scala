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
  ): Unit = {
    val prompt = new PromptConsoleInterpreter[RandomIO](new ui.ConsoleInterpreter[RandomIO])
    val ipd = new IteratedPrisonersDilemma[RandomIO]()

    val generator = new Generator[RandomIO] {
      override def next: StateT[IO, Seed, Long] = StateT { seed: Seed =>
        seed.next[IO].map { next =>
          (next, seed.value)
        }
      }
    }

    val game = new TitForTat(prompt, ipd, generator)
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
  generator: Generator[M]
)(implicit M: Monad[M]) {

  import game._

  type Strategy = game.Strategy

  val interactive = new Strategy {
    override def chose(context: Context, player: Player, opponent: PlayerId): M[Action] = {
      lazy val input: M[Action] =
        prompt.prompt(
          s"""
            |
            | You play against player $opponent. This player's last move was to ${context.getLastMove(player.id, opponent)}
            | Your score is ${context.getScore(player.id)}. Your opponent's score is ${context.getScore(opponent)}
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

  val payoffs: Payoffs = Map(
    (Cooperate, Cooperate) -> (3.0,3.0),
    (Cooperate, Defect) -> (0, 4),
    (Defect, Cooperate) -> (4, 0),
    (Defect, Defect) -> (1,1)
  )

  def randomPlayers(distribution: Seq[(Strategy, Int)]): M[Set[Player]] = {
    val newId = generator.randomInt.map(PlayerId.apply)
    val strategies = distribution.flatMap { case (strategy, number) => List.fill(number)(strategy) }
    val init = M.pure(Set.empty[Player])

    strategies.foldLeft(init) { case (acc, curr) =>
      for {
        participants <- acc
        id <- newId
        player = buildPlayer(id, curr)
      } yield participants + player
    }
  }


  def buildPairings(players: Set[Player]): M[Seq[Pairing]] = {
    val ids = players.toSeq
    ids.foldLeft(M.pure(Seq.empty[Pairing])) { case (acc, curr) =>
      for {
        pairings <- acc
        opponent <- generator.oneOf(ids.filterNot(_ == curr):_*)
      } yield pairings :+ (curr, opponent)
    }
  }


  def randomPairingRounds(rounds: Int)(players: Set[Player]): M[Seq[Pairings]] = {
    val randomPairingRounds = Seq.fill(rounds)(players).map(buildPairings)
    randomPairingRounds.toList.sequence.map(_.toSeq)
  }

  def runGame(players: Set[Player], rounds: Int): M[Map[PlayerId, Score]] = {
    randomPairingRounds(rounds)(players)
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
