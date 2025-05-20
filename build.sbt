//ThisBuild / version := "0.1.0-SNAPSHOT"
//
//ThisBuild / scalaVersion := "2.12.7"
//
//lazy val root = (project in file("."))
//  .settings(
//    name := "akka-course"
//  )
//
//val akkaVersion = "2.5.13"
//
//libraryDependencies ++= Seq(
//  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
//  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
//  "org.scalatest" %% "scalatest" % "3.0.5",
//
//  // Circe for JSON
//  "io.circe" %% "circe-core" % "0.10.0",
//  "io.circe" %% "circe-generic" % "0.10.0",
//  "io.circe" %% "circe-parser" % "0.10.0"
//)
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.17"

lazy val root = (project in file("."))
  .settings(
    name := "akka-course"
  )

val akkaVersion = "2.6.20" // Compatible with Scala 2.12.17
val circeVersion = "0.14.1" // Compatible with Scala 2.12 and stable

libraryDependencies ++= Seq(
  // Akka core (classic)
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion % Test,

  // Circe for JSON
  "io.circe" %% "circe-core" % circeVersion,
  "io.circe" %% "circe-generic" % circeVersion,
  "io.circe" %% "circe-parser" % circeVersion,

  // ScalaTest for unit testing
  "org.scalatest" %% "scalatest" % "3.2.9" % Test
)
