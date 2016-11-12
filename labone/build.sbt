name := "pppl-lab"

organization := "edu.colorado.cs"

version := "3.0.0"

scalaVersion := "2.10.5"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

// Test option: D - show durations
testOptions in Test += Tests.Argument("-oD")

parallelExecution in Test := false
