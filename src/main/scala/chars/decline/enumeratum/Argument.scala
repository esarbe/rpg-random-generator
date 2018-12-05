package chars.decline.enumeratum

import cats.data.ValidatedNel
import enumeratum.{Enum, EnumEntry}
import cats.implicits._
import com.monovore.decline

object Argument {

  def enumArgument[E <: EnumEntry](e: Enum[E]): decline.Argument[E] = new decline.Argument[E] {
    override def read(string: String): ValidatedNel[String, E] =
      e.
        withNameInsensitiveOption(string)
        .toValidNel(s"unexpected value for ${e.toString}: valid values are: ${e.values.map(_.toString.toLowerCase).mkString(", ")}")

    override def defaultMetavar: String = e.toString
  }
}
