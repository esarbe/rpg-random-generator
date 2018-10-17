package chars.app

import cats.data.ValidatedNel
import chars.app.text.PersonDescriptionBuilder
import chars.model.{Culture, Person, Sex}
import chars.random.Generator
import chars.random.model.HumanoidGen
import chars.random.CatsInstances._
import com.monovore.decline.{CommandApp, Opts}
import cats.implicits._
import chars.text.DescriptionPrinter
import com.monovore.decline._
import enumeratum.{Enum, EnumEntry}

import Arguments._

object CommandLine extends CommandApp(
  name = "character-creator",
  header  = "Create Character",
  main = {
    val sex =
      Opts.option[Sex]("sex", help = "character sex", short = "s").orNone

    val culture =
      Opts.option[Culture](long = "culture", help = "character culture", short = "c").orNone

    val seed =
      Opts.option[Long](long = "seed", help = "provide random seed").orNone

    (sex, culture, seed).mapN { case (maybeSex, maybeCulture, maybeSeed) =>
      val sexGen =
        maybeSex.map(s => Generator.constant(s)).getOrElse(Generator.oneOf(Sex.Female, Sex.Male))
      val cultureGen =
        maybeCulture.map(c => Generator.constant(c)).getOrElse(Generator.oneOf(Culture.values:_*))

      val randomPerson =
        for {
          sex <- sexGen
          culture <- cultureGen
          name <- CharacterCreator.buildNameGenerator(sex, culture)
          human <- HumanoidGen.buildGenerator(Generator.constant(sex))
        } yield Person(name, human)

      val seed =
        maybeSeed.getOrElse(new java.util.Random().nextLong())
      val (_, person) = randomPerson(seed)
      println(s"seed: $seed")

      val description = PersonDescriptionBuilder.describe(person)
      println(DescriptionPrinter.print(description))
    }
  }
)


object Arguments {

  def enumArgument[E <: EnumEntry](e: Enum[E]): Argument[E] = new Argument[E] {
    override def read(string: String): ValidatedNel[String, E] =
      e.
        withNameInsensitiveOption(string)
        .toValidNel(s"unexpected value for ${e.toString}: valid values are: ${e.values.map(_.toString.toLowerCase).mkString(", ")}")

    override def defaultMetavar: String = e.toString
  }

  implicit val sexArgument: Argument[Sex] = enumArgument(Sex)
  implicit val cultureArgument: Argument[Culture] = enumArgument(Culture)
}
