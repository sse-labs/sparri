name := "opal-callgraph-miner"

ThisBuild / organization := "org.anon"
ThisBuild / version      := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion := "2.12.15"

lazy val dockerSettings = docker / dockerfile := {

	val artifact: File = assembly.value
	val artifactTargetPath = s"/app/${artifact.name}"

	new Dockerfile {
		from("openjdk:16-jdk")
		add(artifact, artifactTargetPath)
		entryPoint("java", "-jar", "-Xmx8G", "-Xss128m", artifactTargetPath)
	}
}

lazy val mergeStrategySettings = assemblyMergeStrategy := {
	case x: String if x.toLowerCase.contains("manifest.mf") => MergeStrategy.discard
	case x: String if x.toLowerCase.endsWith(".conf") => MergeStrategy.concat
	case x => MergeStrategy.first
}

lazy val root = (project in file("."))
	.aggregate(core, `maven-entity-name-publisher`, `maven-entity-miner`, webapi, `analysis-runner`, evaluation)

lazy val core = (project in file("core"))
	.settings(
		libraryDependencies ++= dependencies.opal,
		libraryDependencies ++= dependencies.slick,
		libraryDependencies ++= Seq(dependencies.scalaTest, dependencies.rabbitMQ, dependencies.akkaStreams,
			dependencies.apacheHttp, dependencies.postgresql, dependencies.jeka, dependencies.mvnarcheologist,
			dependencies.sprayJson)
	)

lazy val `maven-entity-name-publisher` = (project in file("maven-entity-name-publisher"))
	.dependsOn(core)
	.enablePlugins(DockerPlugin)
	.settings(
		libraryDependencies ++= Seq(dependencies.scalaTest, dependencies.rabbitMQ, dependencies.akkaHttp, dependencies.logback,
			dependencies.mvnIndexer),

		assembly / mainClass := Some("org.anon.spareuse.mvnpub.Application"),
		assembly / assemblyJarName := "maven-entity-name-publisher.jar",

		mergeStrategySettings,
		dockerSettings,

		docker / imageNames := Seq(ImageName(s"spar-maven-name-publisher:latest"))
	)

lazy val `maven-entity-miner` = (project in file("maven-entity-miner"))
	.dependsOn(core)
	.enablePlugins(DockerPlugin)
	.settings(
		libraryDependencies ++= Seq(dependencies.scalaTest, dependencies.rabbitMQ, dependencies.logback, dependencies.typesafeConfig),

		assembly / mainClass := Some("org.anon.spareuse.mvnem.Application"),
		assembly / assemblyJarName := "maven-entity-miner.jar",

		mergeStrategySettings,
		dockerSettings,

		docker / imageNames := Seq(ImageName(s"spar-maven-entity-miner:latest"))
	)

lazy val `analysis-runner` = (project in file("analysis-runner"))
	.dependsOn(core)
	.enablePlugins(DockerPlugin)
	.settings(
		libraryDependencies ++= dependencies.opal,
		libraryDependencies ++= Seq(dependencies.scalaTest, dependencies.rabbitMQ, dependencies.logback, dependencies.typesafeConfig),

		assembly / mainClass := Some("org.anon.spareuse.execution.Application"),
		assembly / assemblyJarName := "analysis-runner.jar",

		mergeStrategySettings,
		dockerSettings,

		docker / imageNames := Seq(ImageName(s"spar-analysis-runner:latest"))
	)

lazy val playground = (project in file("playground"))
	.dependsOn(core, evaluation)
	.settings(libraryDependencies ++= Seq(dependencies.logback))

lazy val webapi = (project in file("webapi"))
	.dependsOn(core)
	.enablePlugins(DockerPlugin)
	.settings(
		libraryDependencies ++= Seq(dependencies.akkaStreams, dependencies.akkaHttp, dependencies.akkaActors, dependencies.akkaSprayJson,
			dependencies.logback, dependencies.postgresql, dependencies.rabbitMQ, dependencies.akkaHttpCors),

		assembly / mainClass := Some("org.anon.spareuse.webapi.Application"),
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

		docker / imageNames := Seq(ImageName(s"spar-webapi:latest"))
	)

lazy val evaluation = (project in file("evaluation"))
	.dependsOn(core)
	.enablePlugins(DockerPlugin)
	.settings(
		libraryDependencies ++= Seq(dependencies.logback, dependencies.neo4jDriver),

		assembly / mainClass := Some ("org.anon.spareuse.eval.performance.PerformanceEvaluationApp"),
		assembly / assemblyJarName := "spar-evaluation.jar",
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

		docker / imageNames := Seq(ImageName(s"spar-evaluation:latest"))

	)
	
lazy val dependencies = new {

	val opalVersion = "4.0.0"
	val slickVersion = "3.4.1"

	val opal = Seq(
	  "de.opal-project" % "common_2.12" % opalVersion,
	  "de.opal-project" % "framework_2.12" % opalVersion,
	)
	
	val jeka = "dev.jeka" % "jeka-core" % "0.9.15.RELEASE"
	val mvnarcheologist = "com.squareup.tools.build" % "maven-archeologist" % "0.0.10"

	val mvnIndexer = "org.apache.maven.indexer" % "indexer-reader" % "6.2.2"
	
	val elasticVersion = "7.14.1"
	val elastic = "com.sksamuel.elastic4s" %% "elastic4s-client-esjava" % elasticVersion
	
	val logback = "ch.qos.logback" % "logback-classic" % "1.2.3"
	
	val rabbitMQ = "com.rabbitmq" % "amqp-client" % "5.13.0"
	
	val scalaTest = "org.scalatest" %% "scalatest" % "3.2.9" % "test"

	val apacheHttp = "org.apache.httpcomponents" % "httpclient" % "4.5.13"

	val postgresql = "org.postgresql" % "postgresql" % "42.3.3"
	val slick = Seq(
		"com.typesafe.slick" %% "slick" % slickVersion,
		"com.typesafe.slick" %% "slick-hikaricp" % slickVersion
	)

	val typesafeConfig = "com.typesafe" % "config" % "1.4.2"

	val sprayJson = "io.spray" %% "spray-json" % "1.3.6"

	val akkaVersion = "2.6.18"
	val akkaHttpVersion = "10.2.9"
	val akkaStreams = "com.typesafe.akka" %% "akka-stream" % akkaVersion
	val akkaActors = "com.typesafe.akka" %% "akka-actor" % akkaVersion
	val akkaHttp = "com.typesafe.akka" %% "akka-http" % akkaHttpVersion
	val akkaSprayJson = "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion

	val akkaHttpCors = "ch.megard" %% "akka-http-cors" % "1.1.3"

	val neo4jDriver = "org.neo4j.driver" % "neo4j-java-driver" % "4.3.8"
}
