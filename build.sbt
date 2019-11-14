lazy val scala212 = "2.12.10"
lazy val scala213 = "2.13.1"
lazy val supportedScalaVersions = List(scala213, scala212)

crossScalaVersions := supportedScalaVersions

ThisBuild / organization := "se.bjornregnell"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := scala213

// https://mvnrepository.com/artifact/org.jsoup/jsoup
libraryDependencies += "com.typesafe.akka" %% "akka-http"   % "10.1.10"
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.23"

scalacOptions ++= Seq("-unchecked", "-deprecation")

fork := true
run / connectInput := true
outputStrategy := Some(StdoutOutput)

ThisBuild / useSuperShell := false