name := "scala-on-gears"

version := "0.0.0.1-SNAPSHOT"

scalaVersion := "2.11.5"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.1" % "test"
libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.12.4" % "test"

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-maxSize", "5", "-minSuccessfulTests", "33", "-workers", "1", "-verbosity", "1")
