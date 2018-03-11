package chars.app.ui

import cats.effect._
import cats.FlatMap

trait Console[F[_]] {
  def readLine: F[String]
  def printLine(s: String): F[Unit]
}


trait Prompt[F[_]] {
  def prompt(s: String): F[String]
}


object ConsoleInterpreter extends Console[IO]{
  override def readLine: IO[String] =
    IO { scala.io.StdIn.readLine() }

  override def printLine(s: String): IO[Unit] =
    IO { Console.println(s) }
}

class PromptConsoleInterpreter[F[_]: FlatMap](console: Console[F]) extends Prompt[F] {
  import cats.syntax.flatMap._
  import cats.syntax.functor._

  override def prompt(s: String): F[String] =
    for {
      _ <- console.printLine(s)
      input <- console.readLine
    } yield input
}

