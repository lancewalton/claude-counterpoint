val scala3Version = "3.3.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "claude-counterpoint",
    version := "0.1.0",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.18" % Test
    )
  )