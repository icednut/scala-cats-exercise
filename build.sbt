ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "scala-cats-exercise",
    libraryDependencies ++= Seq("org.typelevel" %% "cats-core" % "2.8.0")
  )
