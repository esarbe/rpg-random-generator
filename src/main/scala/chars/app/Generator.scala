package chars.app

import cats.effect.IO
import chars.app.ui.PromptConsoleInterpreter

import scala.annotation.tailrec


object Generator extends App {
  // mvp:
  //  - main loop with cmd
  //  - command to generate character, option for sex
  //  - command to exit generator

  trait GeneratorAction extends Product with Serializable
  case object GenerateFemale extends GeneratorAction
  case object GenerateMale extends GeneratorAction

  trait Input extends Product with Serializable
  case class GeneratorActionRequested(g: GeneratorAction) extends Input
  case object ExitRequested extends Input
  case object Unknown extends Input

  val prompt = new PromptConsoleInterpreter[IO](ui.ConsoleInterpreter)

  val initialPrompt =
    prompt.prompt(
      """
        |Enter
        |  female  -  to generate a female character
        |  male    -  to generate a male character
        |  exit    -  to close program
      """.stripMargin).map {
      case "female" => GeneratorActionRequested(GenerateFemale)
      case "male" => GeneratorActionRequested(GenerateMale)
      case "exit" => ExitRequested
      case _ => Unknown
    }


  @tailrec
  def mainloop(input: Input): IO[Unit] = input match {
    case ExitRequested => ui.ConsoleInterpreter.printLine("Bye")
    case GeneratorActionRequested(GenerateFemale) =>
      ui.ConsoleInterpreter.printLine("generating female").unsafeRunSync()
      mainloop(initialPrompt.unsafeRunSync())
    case GeneratorActionRequested(GenerateMale) =>
      ui.ConsoleInterpreter.printLine("generating male").unsafeRunSync()
      mainloop(initialPrompt.unsafeRunSync())
    case Unknown =>
      ui.ConsoleInterpreter.printLine("unknown input").unsafeRunSync()
      mainloop(initialPrompt.unsafeRunSync())
  }

  mainloop(initialPrompt.unsafeRunSync())
}


