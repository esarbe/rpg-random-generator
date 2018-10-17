package chars.app.ui

import cats.effect._
import cats.{FlatMap, Monad}
import enumeratum.{Enum, EnumEntry}

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


class PromptConsoleInterpreter[F[_]: FlatMap](console: Console[F]) extends Prompt[F] with Console[F] {
  import cats.syntax.all._

  override def printLine(s: String): F[Unit] = console.printLine(s)
  override def readLine: F[String] = console.readLine

  override def prompt(s: String): F[String] =
    for {
      _ <- console.printLine(s)
      input <- console.readLine
    } yield input
}

object TextInterface {
  def selectEnumPrompt[F[_]: Monad, E <: EnumEntry](enum: Enum[E], prompt: Prompt[F] with Console[F]): F[E] = {
    import cats.syntax.all._

    val options =
      enum
        .lowerCaseNamesToValuesMap
        .map { case (key, value) => s"$key - select $value" }
        .mkString("\n\t", "\n\t", "")

    val text =
      s"""
         |Enter
         |$options
      """.stripMargin

    prompt
      .prompt(text)
      .map(t => enum.withNameInsensitiveOption(t))
      .flatMap {
        case None => prompt.printLine("Unknown input").flatMap(_ => selectEnumPrompt(enum, prompt))
        case Some(v) => v.pure
      }
  }
}