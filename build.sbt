lazy val `txtlog-processor` = (project in file("txtlog-processor")).settings(
  List(
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.5",
      "org.specs2" %% "specs2-core" % "3.8.9" % Test,
      "org.specs2" %% "specs2-matcher-extra" % "3.8.9" % Test
    )
  )
)

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "org.github.il2bostools",
      scalaVersion := "2.12.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "il2-bos-tools"
  ).dependsOn(`txtlog-processor`)
