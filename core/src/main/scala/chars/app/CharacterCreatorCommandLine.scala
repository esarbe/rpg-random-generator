package chars.app

import cats.effect.IO
import cats.implicits._
import chars.app.text.PersonDescriptionBuilder
import chars.app.ui.PromptConsoleInterpreter
import chars.decline.enumeratum.argument._
import chars.decline.random.argument._
import chars.model.{Culture, Person, Sex}
import chars.random._
import chars.text.DescriptionBuilder.DescriptionBuilderOps
import chars.text.{DescriptionBuilder, DescriptionPrinter}
import com.monovore.decline.{CommandApp, Opts, _}


object CharacterCreatorCommandLine extends {
  val generator = RandomGenerator
  val prompt = new PromptConsoleInterpreter[IO](new ui.ConsoleInterpreter)
  implicit val sexArgument: Argument[Sex] = enumArgument(Sex)
  implicit val cultureArgument: Argument[Culture] = enumArgument(Culture)

} with
  CommandApp(
    name = "character-creator",
    header  = "Create Character",
    main = {

    val sex = Opts.option[Sex]("sex", help = "character sex", short = "s")(sexArgument).orNone
    val culture = Opts.option[Culture](long = "culture", help = "character culture", short = "c")(cultureArgument).orNone
    val seed = Opts.option[Seed](long = "seed", help = "provide random seed").orNone

    implicit val personDescriptionBuilder: DescriptionBuilder[Person] = PersonDescriptionBuilder

    (sex, culture, seed).mapN { case (maybeSex, maybeCulture, maybeSeed) =>

      val seed = maybeSeed.getOrElse(Seed(scala.math.random().toLong))

      val person =
        CharacterCreator
          .buildCharacterGenerator(maybeSex, maybeCulture)
          .run(seed).value._2

      val personDescription = DescriptionBuilderOps(person)(personDescriptionBuilder).describe
      println(DescriptionPrinter.print(personDescription))
    }
  }
)
