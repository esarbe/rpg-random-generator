package chars.app

import cats.effect.IO
import cats.implicits._
import chars.app.CharacterCreator.Input.GeneratorActionRequested
import chars.app.text.PersonDescriptionBuilder
import chars.app.ui.PromptConsoleInterpreter
import chars.model._
import chars.random.CatsInstances._
import chars.random.model.HumanoidGen
import chars.random.{Generator, Random}
import chars.text.Description
import chars.text.Description._
import enumeratum._

import scala.annotation.tailrec
import scala.collection.immutable
import scala.io.Source
import scala.util.Try


object CharacterCreator {
  // mvp:
  //  - main loop with cmd
  //  - command to generate character, option for sex
  //  - command to exit generator

  sealed trait GeneratorAction
  case class GenerateCharacter(sex: Sex, culture: Culture) extends GeneratorAction

  sealed trait Input extends EnumEntry
  object Input extends Enum[Input] {
    case class GeneratorActionRequested(g: GeneratorAction) extends Input
    case object ExitRequested extends Input
    case class SetSeed(value: Long) extends Input

    override def values: immutable.IndexedSeq[Input] = findValues
  }


  val prompt = new PromptConsoleInterpreter[IO](ui.ConsoleInterpreter)

  def selectEnumPrompt[E <: EnumEntry](enum: Enum[E]): IO[E] = {
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
      .map(enum.withNameInsensitiveOption)
      .flatMap {
        case None => prompt.printLine("Unknown input").flatMap(_ => selectEnumPrompt(enum))
        case Some(v) => IO(v)
      }

  }

  val sexPrompt: IO[Sex] = selectEnumPrompt(Sex)
  val culturePrompt = selectEnumPrompt(Culture)
  val appPrompt =
    prompt
      .prompt(
        s"""
         | please chose (or exit):
       """.stripMargin)
    .map{
      case "exit" => sys.exit()
    }

  val cPrompt: IO[GeneratorActionRequested] =
    for {
      sex <- sexPrompt
      culture <- culturePrompt
    } yield GeneratorActionRequested(GenerateCharacter(sex, culture))


  def buildNameGenerator(sex: Sex, culture: Culture): Random[String] = {

    val file = s"names/${culture.entryName.toLowerCase}-${sex.entryName.toLowerCase}.txt"
    val resource = Source.fromResource(file)

    val names =
      Try(resource.getLines.toSeq)
        .toOption
        .getOrElse(sys.error(s"resource ${culture.entryName}-$sex.txt not found"))

    Generator.oneOf(names:_*)
  }


  def sexGenerator(sex: Sex): Random[Sex] =
    Option(sex)
      .filter(Set[Sex](Sex.Male, Sex.Female).contains)
      .map(s => Generator.constant(s))
      .getOrElse(Generator.oneOf(Sex.Male, Sex.Female))


  import ui.ConsoleInterpreter._

  case class State(maybeSeed: Option[Long])

  @tailrec
  def mainloop(state: State, input: Input): IO[Unit] = input match {
    case Input.ExitRequested =>
      printLine("Bye")
      sys.exit

    case GeneratorActionRequested(GenerateCharacter(sex, culture)) =>

      val randomPerson =
        for {
          sex <- sexGenerator(sex)
          name <- buildNameGenerator(sex, culture)
          human <- HumanoidGen.buildGenerator(Generator.constant(sex))
        } yield Person(name, human)

      val seed = state.maybeSeed.getOrElse(new java.util.Random().nextLong())
      val (nextSeed, person) = randomPerson.apply(seed)

      printLine(person.toString).unsafeRunSync()
      printLine(s"character $seed").unsafeRunSync()
      printLine(pd(PersonDescriptionBuilder.describe(person))).unsafeRunSync()

      mainloop(state.copy(maybeSeed = Some(nextSeed)), appPrompt.unsafeRunSync())
    case Input.SetSeed(value) =>
      printLine(s"random seed set to $value")
      mainloop(state.copy(maybeSeed = Some(value)), appPrompt.unsafeRunSync())
    }



  def pd(description: Description): String = pd(description, 0)
  def pd(description: Description, indentation: Int = 0): String = description match {
      case Leaf(label, value) =>
        "\n" + "\t" * indentation + s"$label: $value"
      case Node(label, descriptions) =>
        "\n" + "\t" * indentation + s"$label:" +
          descriptions.map(pd(_, indentation + 1)).mkString
  }

}
