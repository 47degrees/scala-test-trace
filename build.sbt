ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"
ThisBuild / crossScalaVersions := Seq("2.13.10", "3.2.1")

lazy val root = (project in file("."))
  .settings(
    name := "scalacheck-trace",
    scalacOptions ++= (CrossVersion.partialVersion(scalaVersion.value) match {
      case Some((2, _)) => Seq("-Xsource:3")
      case _ => Seq()
    }),
    idePackagePrefix := Some("com.xebia.functional"),
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.17.0"
  )
