package chars.decline.enumeratum

import cats.data.ValidatedNel
import com.monovore.decline.Argument
import enumeratum.{Enum, EnumEntry}
import cats.implicits._

object Argument {

  def enumArgument[E <: EnumEntry](e: Enum[E]): Argument[E] = new Argument[E] {
    override def read(string: String): ValidatedNel[String, E] =
      e.
        withNameInsensitiveOption(string)
        .toValidNel(s"unexpected value for ${e.toString}: valid values are: ${e.values.map(_.toString.toLowerCase).mkString(", ")}")

    override def defaultMetavar: String = e.toString
  }
}
