import sbt._
import sbt.Keys._

object Settings {

  val httpsVersion = "0.18.21"

  val dependencies =
    libraryDependencies ++= Seq(
      "org.scalactic" %% "scalactic" % "3.0.0",
      "org.scalatest" %% "scalatest" % "3.0.0" % "test",
      "org.typelevel" %% "cats-core" % "1.0.1" withSources(),
      "org.typelevel" %% "cats-effect" % "0.5" withSources(),
      "org.scalacheck" %% "scalacheck" % "1.13.4" % "test",
      "com.beachape" %% "enumeratum" % "1.5.12",
      "com.monovore" %% "decline" % "0.5.0",
      "com.github.mpilquist" %% "simulacrum" % "0.10.0",
      "io.estatico" %% "newtype" % "0.4.2",
      "ch.qos.logback" % "logback-classic" % "0.9.24"
    ) ++ Seq(
      "org.http4s"%% "http4s-blaze-server" % httpsVersion,
      "org.http4s" %% "http4s-circe" % httpsVersion,
      "org.http4s" %% "http4s-dsl" % httpsVersion
    )
}
