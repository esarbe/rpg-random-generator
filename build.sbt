name := "RPG Character Generator"

scalacOptions += "-Ypartial-unification"

val httpsVersion = "0.18.21"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6")

addCompilerPlugin("com.olegpy" %% "better-monadic-for"  % "0.2.4")

enablePlugins(JavaAppPackaging)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1" withSources()

libraryDependencies += "org.typelevel" %% "cats-effect" % "0.5" withSources()

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.13.4" % "test"

libraryDependencies += "com.beachape" %% "enumeratum" % "1.5.12"

libraryDependencies += "com.monovore" %% "decline" % "0.5.0"

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.10.0"

libraryDependencies += "io.estatico" %% "newtype" % "0.4.2"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "0.9.24"


libraryDependencies ++= Seq(
  "org.http4s"%% "http4s-blaze-server" % httpsVersion,
  "org.http4s" %% "http4s-circe" % httpsVersion,
  "org.http4s" %% "http4s-dsl" % httpsVersion
)

version := "0.0.1-SNAPSHOT"

scalaVersion := "2.12.4"

