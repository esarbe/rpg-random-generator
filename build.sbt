import sbt.Keys.version
import sbt.addCompilerPlugin

val root =
  (project in file("core"))
    .settings(
      name := "RPG Character Generator",
      scalacOptions += "-Ypartial-unification",
      version := "0.0.1-SNAPSHOT",
      scalaVersion := "2.12.4",
      addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full),
      addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6"),
      addCompilerPlugin("com.olegpy" %% "better-monadic-for"  % "0.2.4"),
      Settings.dependencies,
    ).enablePlugins(JavaAppPackaging)












