ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"

lazy val root = (project in file("."))
  .settings(
    name := "testScalaIor",
    libraryDependencies += "org.typelevel" %% "cats-core" % "2.13.0",
    idePackagePrefix := Some("org.qadusch.example")
  )
