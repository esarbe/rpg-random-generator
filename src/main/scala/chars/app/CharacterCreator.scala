package chars.app

import cats.effect.IO
import cats.implicits._
import chars.app.CharacterCreator.Input.GeneratorActionRequested
import chars.app.text.PersonDescriptionBuilder
import chars.app.ui.{PromptConsoleInterpreter, TextInterface}
import chars.model._
import chars.cats.CatsInstances._
import chars.random.model.Human
import chars.random.{Generator, Random}
import chars.text.DescriptionPrinter
import enumeratum._

import scala.annotation.tailrec
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


  val prompt = new PromptConsoleInterpreter[IO](ui.ConsoleInterpreter)

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
    val sexGen = maybeSex.map(s => Generator.constant(s)).getOrElse(Generator.oneOf(Sex.Female, Sex.Male))
    val cultureGen = maybeCulture.map(c => Generator.constant(c)).getOrElse(Generator.oneOf(Culture.values:_*))

    for {
      sex <- sexGen
      culture <- cultureGen
      name <- buildNameGenerator(sex, culture)
      human <- Human.buildGenerator(Generator.constant(sex))
    } yield Person(name, human)
  }


  def buildNameGenerator(sex: Sex, culture: Culture): Random[String] = {

    val file = s"names/${culture.entryName.toLowerCase}-${sex.entryName.toLowerCase}.txt"
    val resource = Source.fromResource(file)

    val names =
      Try(resource.getLines.toSeq)
        .toOption
        .getOrElse(sys.error(s"resource ${culture.entryName}-$sex.txt not found"))

    Generator.oneOf(names:_*)
  }


  def sexGenerator(maybeSex: Option[Sex]): Random[Sex] =
    maybeSex
      .filter(Set[Sex](Sex.Male, Sex.Female).contains)
      .map(s => Generator.constant(s))
      .getOrElse(Generator.oneOf(Sex.Male, Sex.Female))


  import ui.ConsoleInterpreter._
}
