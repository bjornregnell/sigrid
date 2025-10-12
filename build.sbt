ThisBuild / scalaVersion := "3.3.6" // Current LTS (as of 2025-10-12)

lazy val common = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Pure)
  .in(file("common"))
  .settings(
    name := "common"
  )

lazy val commonJvm = common.jvm
lazy val commonJs = common.js

lazy val client = (project in file("client"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(commonJs)
  .settings(
    name := "client",
    libraryDependencies ++= Seq(
      "com.raquo" %%% "laminar" % "17.2.1", // Latest as of 2025-10-12
      "io.github.cquiroz" %%% "scala-java-time" % "2.6.0" // java.time Scala.js compatability
    ),
    scalaJSUseMainModuleInitializer := true
  )

lazy val server = (project in file("server"))
  .dependsOn(commonJvm)
  .settings(
    name := "server",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "cask" % "0.9.7", // Latest as of 2025-10-12
      "storky" % "storky" % "1.0.0" from "https://github.com/bjornregnell/storky/releases/download/v1.0.0/storky_3-1.0.0.jar"
    )
  )
