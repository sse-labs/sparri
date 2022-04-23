name := "opal-callgraph-miner"

ThisBuild / organization := "de.tudo"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.15"

lazy val root = (project in file("."))
	.aggregate(common, extractor, webapi)

lazy val common = (project in file("common"))
	.settings(
		libraryDependencies ++= dependencies.opal,
		libraryDependencies ++= Seq(dependencies.logback, dependencies.rabbitMQ, dependencies.jeka, dependencies.mvnarcheologist,
			dependencies.scalaTest, dependencies.akkaStreams, dependencies.apacheHttp)
	)

lazy val extractor = (project in file("classfile-feature-extractor"))
	.dependsOn(common)
	.enablePlugins(DockerPlugin)
	.settings(
		libraryDependencies ++= Seq(dependencies.jeka, dependencies.elastic, dependencies.logback, dependencies.rabbitMQ,
			dependencies.scalaTest, dependencies.mvnarcheologist, dependencies.postgresql),
		libraryDependencies ++= dependencies.opal,

		assembly / mainClass := Some("de.tudo.sse.classfilefeatures.extractor.Application"),
		assembly / assemblyJarName := "cf-feature-extractor.jar",
		assemblyMergeStrategy := {
			case x: String if x.toLowerCase.contains("manifest.mf") => MergeStrategy.discard
			case x: String if x.toLowerCase.endsWith(".conf") => MergeStrategy.concat
			case x => MergeStrategy.first
		},

		docker / dockerfile := {

			val artifact: File = assembly.value
			val artifactTargetPath = s"/app/${artifact.name}"

			new Dockerfile {
				from("openjdk:16-jdk")
				add(artifact, artifactTargetPath)
				entryPoint("java", "-jar", "-Xmx8G", "-Xss128m", artifactTargetPath)
			}
		},

		docker / imageNames := Seq(ImageName(s"cf-feature-extractor:latest"))
	)

lazy val webapi = (project in file("webapi"))
	.dependsOn(common)
	.enablePlugins(DockerPlugin)
	.settings(
		libraryDependencies ++= Seq(dependencies.akkaStreams, dependencies.akkaHttp, dependencies.akkaActors, dependencies.akkaSprayJson,
			dependencies.logback, dependencies.postgresql, dependencies.rabbitMQ),

		assembly / mainClass := Some("de.tudo.sse.classfilefeatures.webapi.Application"),
		assembly / assemblyJarName := "cf-webapi.jar",
		assemblyMergeStrategy := {
			case x: String if x.toLowerCase.contains("manifest.mf") => MergeStrategy.discard
			case x: String if x.toLowerCase.endsWith(".conf") => MergeStrategy.concat
			case x => MergeStrategy.first
		},

		docker / dockerfile := {

			val artifact: File = assembly.value
			val artifactTargetPath = s"/app/${artifact.name}"

			new Dockerfile {
				from("openjdk:16-jdk")
				add(artifact, artifactTargetPath)
				entryPoint("java", "-jar", "-Xmx8G", "-Xss128m", artifactTargetPath)
			}
		},

		docker / imageNames := Seq(ImageName(s"cf-webapi:latest"))
	)
	
lazy val dependencies = new {

	val opalVersion = "4.0.0"
	val opal = Seq(
	  "de.opal-project" % "common_2.12" % opalVersion,
	  "de.opal-project" % "framework_2.12" % opalVersion,
	)
	
	val jeka = "dev.jeka" % "jeka-core" % "0.9.15.RELEASE"
	val mvnarcheologist = "com.squareup.tools.build" % "maven-archeologist" % "0.0.10"
	
	val elasticVersion = "7.14.1"
	val elastic = "com.sksamuel.elastic4s" %% "elastic4s-client-esjava" % elasticVersion
	
	val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"
	
	val rabbitMQ = "com.rabbitmq" % "amqp-client" % "5.13.0"
	
	val scalaTest = "org.scalatest" %% "scalatest" % "3.2.9" % "test"

	val apacheHttp = "org.apache.httpcomponents" % "httpclient" % "4.5.13"

	val postgresql = "org.postgresql" % "postgresql" % "42.3.3"


	val akkaVersion = "2.6.18"
	val akkaHttpVersion = "10.2.9"
	val akkaStreams = "com.typesafe.akka" %% "akka-stream" % akkaVersion
	val akkaActors = "com.typesafe.akka" %% "akka-actor" % akkaVersion
	val akkaHttp = "com.typesafe.akka" %% "akka-http" % akkaHttpVersion
	val akkaSprayJson = "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion
}
