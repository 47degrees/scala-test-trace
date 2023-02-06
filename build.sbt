ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.2"

lazy val root = (project in file("."))
  .settings(
    name := "scalacheck-trace",
    idePackagePrefix := Some("com.xebia.functional"),
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0"
  )
