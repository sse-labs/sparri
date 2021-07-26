name := "incremental-cg-creator-producer"

version := "0.1"

scalaVersion := "2.12.14"

enablePlugins(DockerPlugin)

assembly / mainClass := Some("org.tud.cgcrawling.LibraryIdentifierProducer")
assembly / assemblyJarName := "cg-crawler-producer.jar"

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
    from("openjdk:8-jre-alpine")
    add(artifact, artifactTargetPath)
    entryPoint("java", "-jar", "-Xms8G", "-Xmx8G", artifactTargetPath)
  }
}

docker / imageNames := Seq(
  // Sets the latest tag
  ImageName(s"${name.value}:latest"),
)

libraryDependencies += "com.typesafe.akka" %% "akka-stream" % "2.6.15"

libraryDependencies += "org.apache.maven.indexer" % "indexer-reader" % "6.0.0"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "com.rabbitmq" % "amqp-client" % "5.13.0"