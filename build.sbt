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
      "com.raquo" %%% "laminar" % "17.2.1" // Latest as of 2025-10-12
    ),
    scalaJSUseMainModuleInitializer := true
  )

lazy val server = (project in file("server"))
  .dependsOn(commonJvm)
  .settings(
    name := "server",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "cask" % "0.9.7" // Latest as of 2025-10-12
    )
  )
