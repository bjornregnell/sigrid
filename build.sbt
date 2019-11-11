scalaVersion := "2.13.1"
// https://mvnrepository.com/artifact/org.jsoup/jsoup
libraryDependencies += "com.typesafe.akka" %% "akka-http"   % "10.1.10"
libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.5.23"

scalacOptions ++= Seq("-unchecked", "-deprecation")

fork := true
run / connectInput := true
outputStrategy := Some(StdoutOutput)
