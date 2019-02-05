package chars.app

import java.util.concurrent.atomic.AtomicReference

import cats.data.StateT
import cats.effect.IO
import chars.random.{Generator, Seed}
import chars.titfortat.PrisonersDilemma.Action
import chars.titfortat.service.PlayerInteraction
import chars.titfortat.{IteratedPrisonersDilemma, PrisonersDilemma, TitForTat}
import fs2.StreamApp
import org.http4s.dsl.impl.Root
import org.http4s.dsl.io._
import org.http4s.headers.Location
import org.http4s.server.blaze.BlazeBuilder
import org.http4s.{HttpService, MediaType, Uri, headers}

import scala.concurrent.Promise
import scala.util.Success


object TitForTatWebservice extends StreamApp[IO] {

  type RandomIO[T] = StateT[IO, Seed, T]

  val ref = new AtomicReference(Promise[Action])
  val message = new AtomicReference(None: Option[String])

  val playerInteraction = new PlayerInteraction[RandomIO] {
    override def askForUserAction(
      playerId: PrisonersDilemma.PlayerId,
      context: IteratedPrisonersDilemma[RandomIO]#ContextImp
    ): RandomIO[Action] = {

      val opponentsLastMove = context.getLastMove(playerId, context.opponent)
      val opponentsScore = context.getScore(context.opponent)

      val m =
        s"""<html><body>
          | You play against player ${context.opponent}. This player's last move was to $opponentsLastMove
          | Your score is ${context.getScore(playerId)}. Your opponent's score is $opponentsScore
          | What do you chose to do? <a href="/player/cooperate">cooperate</a> or
          | <a href="/player/defect">defect</a>?
          | </body>
          | </html>
        """.stripMargin

      message.set(Some(m))

      StateT.apply[IO, Seed, Action]{
        i =>
          IO.fromFuture(IO {
            val p = Promise[Action]
            ref.set(p)
            p.future
          }).map(a => (i, a))}
    }
  }

  val ipd = new IteratedPrisonersDilemma[RandomIO]()

  val generator = new Generator[RandomIO] {
    override def next: StateT[IO, Seed, Long] = StateT { seed: Seed =>
      seed.next[IO].map { next =>
        (next, seed.value)
      }
    }
  }


  val game = new TitForTat(ipd, generator, playerInteraction)
  val rounds = 10

  val distribution =
    Seq(
      game.game.titForTat -> 60,
      game.game.defect -> 10,
      game.game.cooperate -> 30,
      game.interactive -> 1
    )

  val endState =
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
        .map { case (id, score) => s"<li>$id (${playerById(id).strategy})\t$score</li>" }
        .mkString("\n")

      endState <- StateT.apply[IO, Seed, Unit]( i=> IO((i, message.set(Some(
        s"""
           | <h1>Final scores</h1>
           | <ul>
           | $displayScores
           | </ul>
           |
           | "Thank you for playing."

            """.stripMargin)))))
    } yield endState


  val playerService = HttpService[IO] {
    case GET -> Root / "player" =>
      Ok(message.get().getOrElse(""), headers.`Content-Type`(MediaType.`text/html`))
    case GET -> Root / "player" / "defect" =>
      ref.get().complete(Success(Action.Defect))
      TemporaryRedirect(Location(Uri.uri("/player")))
    case GET -> Root / "player" / "cooperate" =>
      ref.get().complete(Success(Action.Cooperate))
      TemporaryRedirect(Location(Uri.uri("/player")))
  }

  import concurrent.ExecutionContext.Implicits.global

  override def stream(
    args: List[String],
    requestShutdown: IO[Unit]
  ): fs2.Stream[IO, StreamApp.ExitCode] = {
    endState.runA(Seed(100)).unsafeToFuture()
    BlazeBuilder[IO]
      .bindHttp(Option(System.getenv("PORT")).map(Integer.parseInt).getOrElse(sys.error("no port set")), "0.0.0.0")
      .mountService(playerService, "/")
      .serve
  }
}
