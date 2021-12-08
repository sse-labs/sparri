name := "reachable-method-analysis-core"

version := "0.1"

scalaVersion := "2.12.15"

val opalVersion = "4.0.0"
libraryDependencies ++= Seq(
  "de.opal-project" % "common_2.12" % opalVersion,
  "de.opal-project" % "framework_2.12" % opalVersion,
)

libraryDependencies += "org.neo4j.driver" % "neo4j-java-driver" % "4.3.1"

val elastic4sVersion = "7.14.1"
libraryDependencies ++= Seq(
  "com.sksamuel.elastic4s" %% "elastic4s-client-esjava" % elastic4sVersion,
  "com.sksamuel.elastic4s" %% "elastic4s-client-akka" % elastic4sVersion
)

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

val akkaHttpVersion = "10.1.11"

libraryDependencies += "com.typesafe.akka" %% "akka-http" % akkaHttpVersion
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

