package chars.decline.random

import cats.data.ValidatedNel
import chars.random.Seed
import com.monovore.decline.Argument

import cats.implicits._

import scala.util.Try

object argument {
  implicit val seedArgument: Argument[Seed] = new Argument[Seed] {
    override def read(string: String): ValidatedNel[String, Seed] =
      Try(string.toLong)
        .map(Seed.apply)
        .toEither
        .left.map(_.getMessage)
        .toValidatedNel

    override def defaultMetavar: String = "random seed"
  }

}
