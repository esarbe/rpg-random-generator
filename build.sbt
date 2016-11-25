name := "RPG Character Generator"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.0"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % "test"

libraryDependencies += "org.typelevel" %% "cats" % "0.7.2"

libraryDependencies += "io.rbricks" %% "itemized" % "0.2-SNAPSHOT"

libraryDependencies += "com.github.mpilquist" %% "simulacrum" % "0.10.0"

version := "0.0.0"

scalaVersion := "2.11.8"

