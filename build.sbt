import sbt._

lazy val core =
  (project in file("core"))
    .settings(
      Settings.common,
      Dependencies.common,
      Dependencies.cats
    )

lazy val services =
  (project in file("services"))
    .settings(
      Settings.common,
      Dependencies.common,
    )

lazy val web =
  (project in file("web"))
  .settings(
    Settings.common,
    Dependencies.common,
    Dependencies.web,
  ).dependsOn(services, core)

val root =
  (project in file("."))
    .settings(
      organization := "org.esarbe",
      version := "0.0.1-SNAPSHOT",
      name := "chars",
      mainClass in Compile := Some("chars.app.TitForTatWebservice")
    )
    .dependsOn(core, services, web)
    .enablePlugins(JavaAppPackaging)
