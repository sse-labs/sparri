name := "reachable-method-analysis-core"

version := "0.1"

scalaVersion := "2.12.15"

publishM2Configuration := publishM2Configuration.value.withOverwrite(true)

val opalVersion = "4.0.0"
libraryDependencies ++= Seq(
  "de.opal-project" % "common_2.12" % opalVersion exclude("com.fasterxml.jackson.core", "jackson-annotations"),
  "de.opal-project" % "framework_2.12" % opalVersion,
)

libraryDependencies += "org.neo4j.driver" % "neo4j-java-driver" % "4.3.1"

val elastic4sVersion = "7.14.1"
libraryDependencies ++= Seq(
  "com.sksamuel.elastic4s" %% "elastic4s-client-esjava" % elastic4sVersion,
  "com.sksamuel.elastic4s" %% "elastic4s-client-akka" % elastic4sVersion
)

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

