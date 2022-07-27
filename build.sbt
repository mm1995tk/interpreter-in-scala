val scala3Version = "3.1.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "interpreter-in-scala",
    version := "0.1.0-SNAPSHOT",
    scalaVersion := scala3Version,
    libraryDependencies ++= Seq(
      "org.scalameta" %% "munit" % "0.7.29" % Test,
      "org.atnos" %% "eff" % "6.0.1"
    ),
    scalacOptions ++= Seq("-Ykind-projector")
  )
