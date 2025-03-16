val scala3Version = "3.3.5"

lazy val root = project
  .in(file("."))
  .settings(
    name := "Bot-tender",
    version := "0.3.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= List(
      "org.scalameta" %% "munit" % "1.1.0" % Test,
      "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
    ),
  )
