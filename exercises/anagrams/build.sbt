val scala3Version = "3.3.5"

lazy val root = project
  .in(file("."))
  .settings(
    name := ".",
    version := "2025.02-SNAPSHOT",

    scalaVersion := scala3Version,
  )
