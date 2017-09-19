name := "baseball-event-source"

version := "1.0"

scalaVersion := "2.12.3"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.0.1" % "test",
  "com.typesafe.akka" %% "akka-actor"  % "2.5.3",
  "com.typesafe.akka" %% "akka-stream" % "2.5.3"
)
