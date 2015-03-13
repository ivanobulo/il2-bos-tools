name := "il2bos-control"

resolvers += "Scalaz Bintray Repo" at "http://dl.bintray.com/scalaz/releases"

lazy val commonSettings = Seq(
  version := "1.0-SNAPSHOT",
  scalaVersion := "2.11.6",
  organization := "org.github.ivanobulo",
  libraryDependencies ++= Seq("org.specs2" %% "specs2-core" % "3.0" % "test")
)

lazy val txtLogProcLib = (project in file("txtlog-processor")).
  settings(commonSettings: _*).
  settings(
    libraryDependencies ++= Seq(
      "org.specs2" %% "specs2-matcher-extra" % "3.0" % "test",
      "org.parboiled" %% "parboiled" % "2.1.0",
      "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.3"
    )
  )

lazy val root = (project in file(".")).
  aggregate(txtLogProcLib)