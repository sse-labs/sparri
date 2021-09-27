name := "incremental-cg-creator-consumer"

version := "0.1"

scalaVersion := "2.12.14"

enablePlugins(DockerPlugin)

assembly / mainClass := Some("org.tud.cgcrawling.CallGraphCrawler")
assembly / assemblyJarName := "cg-crawler-consumer.jar"

assemblyMergeStrategy := {
  case x: String if x.toLowerCase.contains("manifest.mf") => MergeStrategy.discard
  case x: String if x.toLowerCase.endsWith(".conf") => MergeStrategy.concat
  case x => MergeStrategy.first
}

docker / dockerfile := {
  // The assembly task generates a fat JAR file
  val artifact: File = assembly.value
  val artifactTargetPath = s"/app/${artifact.name}"

  new Dockerfile {
    from("openjdk:8-jre")
    add(artifact, artifactTargetPath)
    entryPoint("java", "-jar", "-Xms8G", "-Xmx8G", artifactTargetPath)
  }
}

docker / imageNames := Seq(
  // Sets the latest tag
  ImageName(s"${name.value}:latest"),
)

val opalVersion = "4.0.0"
libraryDependencies ++= Seq(
  "de.opal-project" % "common_2.12" % opalVersion,
  "de.opal-project" % "framework_2.12" % opalVersion,
)

val akkaVersion = "2.6.15"
val akkaHttpVersion = "10.1.11"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion
)

libraryDependencies += "org.neo4j.driver" % "neo4j-java-driver" % "4.3.1"

val elastic4sVersion = "7.14.1"
libraryDependencies ++= Seq(
  "com.sksamuel.elastic4s" %% "elastic4s-client-esjava" % elastic4sVersion,
  "com.sksamuel.elastic4s" %% "elastic4s-client-akka" % elastic4sVersion
)

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "com.rabbitmq" % "amqp-client" % "5.13.0"


val aetherVersion = "1.1.0"
libraryDependencies ++= Seq(
  "org.eclipse.aether" % "aether-api" % aetherVersion,
  "org.eclipse.aether" % "aether-impl" % aetherVersion,
  "org.eclipse.aether" % "aether-transport-http" % aetherVersion,
  "org.eclipse.aether" % "aether-transport-file" % aetherVersion,
  "org.eclipse.aether" % "aether-connector-basic" % aetherVersion
)

libraryDependencies ++= Seq(
  "org.apache.maven" % "maven-aether-provider" % "3.1.0",
  "org.apache.maven" % "maven-artifact" % "3.8.1"
)
