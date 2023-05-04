# SPARRI
This repository holds a Scala-based implementation of the Static Program Analysis Result Reuse Index (SPARRI). In order to compile and run SPARRI, you will need:
- JDK 1.8
- SBT (Scala Build Tool)
- Docker

## Overview
SPARRI consists of multiple components, each of which is realized as a separate Scala module:
- `./analysis-runner` Component that executes built-in analyses on demand
- `./core` Core functionality shared among SPARRI components
- `./evaluation` Evaluation as reported in our paper
- `./maven-entity-miner` Component that indexes programs (GAV-Triples) from Maven Central on demand
- `./maven-entity-name-publisher` Component that requests indexing of all Maven Central programs
- `./webapi` Implementation of an RESTful HTTP API

Furthermore, this repository contains:
- `./playground` A Scala project for testing SPARRI functionality
- `./literature-review` Directory holding all definitions and results of our literature survey on SPA implementations

## Getting Started
For SPARRI to work you will need instances of RabbitMQ and Postgres available. They can be spawned by executing:
```
docker run -d --name sparri-mq -e RABBITMQ_DEFAULT_USER=<USER> -e RABBITMQ_DEFAULT_PASS=<PASS> -p 9000:5672 rabbitmq:3.10

docker run -d --name sparri-db -e POSTGRES_USER=<USER> -e POSTGRES_PASSWORD=<PASS> -e POSTGRES_DB=spa-results -p 9001:5432 postgres:14.2
```
Note that you will need to expose the RabbitMQ port `5672` on your machine (in this example the host's port `9000` is used). The same is true for port `5432` of the Postgres container (here: host port `9001`).

TODO CONFIGURATION

Now you can build all SPARRI components by executing `sbt clean docker` in the root directory of this repository.