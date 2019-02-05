package chars.app

import cats.Id
import cats.data.State
import cats.effect.IO
import cats.implicits._
import chars.app.CharacterCreator.Input.GeneratorActionRequested
import chars.app.ui.{PromptConsoleInterpreter, TextInterface}
import chars.model._
import chars.random.{Generator, Random, RandomGenerator}
import enumeratum._

import scala.collection.immutable
import scala.io.Source
import scala.util.Try


object CharacterCreator {

  sealed trait GeneratorAction
  case class GenerateCharacter(sex: Sex, culture: Culture) extends GeneratorAction

  sealed trait Input extends EnumEntry
  object Input extends Enum[Input] {
    case class GeneratorActionRequested(g: GeneratorAction) extends Input
    case object ExitRequested extends Input
    case class SetSeed(value: Long) extends Input

    override def values: immutable.IndexedSeq[Input] = findValues
  }

  val prompt = new PromptConsoleInterpreter[IO](new ui.ConsoleInterpreter)
  val generator = RandomGenerator

  val sexPrompt: IO[Sex] = TextInterface.selectEnumPrompt(Sex, prompt)
  val culturePrompt = TextInterface.selectEnumPrompt(Culture, prompt)
  val appPrompt =
    prompt
      .prompt(
        s"""
         | please chose (or exit):
         | char - to generate a character
       """.stripMargin)
    .map {
      case "exit" => Input.ExitRequested

    }

  val cPrompt: IO[GeneratorActionRequested] =
    for {
      sex <- sexPrompt
      culture <- culturePrompt
    } yield GeneratorActionRequested(GenerateCharacter(sex, culture))


  def buildCharacterGenerator(
    maybeSex: Option[Sex],
    maybeCulture: Option[Culture]
  ): Random[Person] = {
    val sexGen = maybeSex.map(s => generator.constant(s)).getOrElse(generator.oneOf(Sex.Female, Sex.Male))
    val cultureGen = maybeCulture.map(c => generator.constant(c)).getOrElse(generator.oneOf(Culture.values:_*))
    val humanGen = new chars.random.model.Human[Random](generator)

    for {
      sex <- sexGen
      culture <- cultureGen
      name <- buildNameGenerator(sex, culture)
      human <- humanGen.buildGenerator(generator.constant(sex))
    } yield Person(name, human)
  }


  def buildNameGenerator(sex: Sex, culture: Culture): Random[String] = {

    val file = s"names/${culture.entryName.toLowerCase}-${sex.entryName.toLowerCase}.txt"
    val resource = Source.fromResource(file)

    val names =
      Try(resource.getLines.toSeq)
        .toOption
        .getOrElse(sys.error(s"resource ${culture.entryName}-$sex.txt not found"))

    generator.oneOf(names:_*)
  }


  def sexGenerator(maybeSex: Option[Sex]): Random[Sex] =
    maybeSex
      .filter(Set[Sex](Sex.Male, Sex.Female).contains)
      .map(s => generator.constant(s))
      .getOrElse(generator.oneOf(Sex.Male, Sex.Female))
}
