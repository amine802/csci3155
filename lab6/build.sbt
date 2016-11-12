name := "pppl-lab"

organization := "edu.colorado.cs"

version := "3.0.0"

scalaVersion := "2.10.5"

libraryDependencies += "org.scalatest" % "scalatest_2.10" % "2.0" % "test"

// set logging to show only errors during runs
logLevel in run := Level.Error

logLevel in runMain := Level.Error

// set scalatest options: -o standard output, D durations
testOptions in Test += Tests.Argument("-oD")

parallelExecution in Test := false
