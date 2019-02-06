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
    .enablePlugins(JavaAppPackaging)
    .aggregate(core, services, web)
