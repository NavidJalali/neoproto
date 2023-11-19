ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "3.3.1"
ThisBuild / organization := "io.navidjalali"

val Versions = new {
  val cats = "2.6.1"
}

lazy val root = (project in file("."))
  .settings(
    name                                   := "neoproto",
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % Versions.cats
    )
    scalacOptions ++= Seq(
      "-source:future",
      "-Ykind-projector",
      "-Xcheck-macros",
      "-Xfatal-warnings",
      "-deprecation",
      "-Wunused:imports"
    )
  )
