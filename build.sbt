ThisBuild / organization := "se.bjornregnell"
ThisBuild / version      := "0.2.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.13.9"

libraryDependencies += "com.typesafe.akka" %% "akka-http"   % "10.2.10"
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.20"//"2.5.23"

scalacOptions ++= Seq("-unchecked", "-deprecation")

fork := true
run / connectInput := true
outputStrategy := Some(StdoutOutput)

ThisBuild / useSuperShell := false