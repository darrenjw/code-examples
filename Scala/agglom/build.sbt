name := "agglom"

version := "0.1-SNAPSHOT"

scalacOptions ++= Seq(
  "-unchecked", "-deprecation", "-feature"
)

libraryDependencies  ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.16" % "test",
  "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
)

scalaVersion := "3.3.0"

fork := true


