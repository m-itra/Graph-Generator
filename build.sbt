ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.6"

lazy val root = (project in file("."))
  .settings(
    name := "Graph-Generator",
    libraryDependencies ++= Seq(
      "com.lihaoyi" %% "upickle" % "3.1.3",
      "com.lihaoyi" %% "os-lib"  % "0.9.1",
    )
  )
