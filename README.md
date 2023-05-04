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

Now you have to configure SPARRI to know where the message queue and database are located. To do so, open the file `./core/src/main/resources/application.conf` and replace all occurrances of the term `<CHANGEME>` with the respective values. You will need to enter the host name (`serverName` and `mq-host`), the ports for the database (`portNumber`) and RabbitMQ (`mq-port`), as well as the credentials for the database (`user` and `password`) and RabbitMQ (`mq-user` and `mq-pass`).

```
pa-reuse.postgres {
    connectionPool = "HikariCP"
    initializationFailTimeout = 2000
    dataSourceClass = "org.postgresql.ds.PGSimpleDataSource"

    properties = {
        serverName="<CHANGEME>"
        portNumber="<CHANGEME>"
        databaseName="spa-results"
        user="<CHANGEME>"
        password="<CHANGEME>"
    }

    numThreads = 8
}

spa-reuse.mq {
    mq-user = "<CHANGEME>"
    mq-pass = "<CHANGEME>"
    mq-host = "<CHANGEME>"
    mq-port = 0 #ATTENTION: ALSO REPLACE THIS VALUE!

    analysis-queue-name = "mvn-analyses"
    analysis-queue-exchange-name = "analyses-exchange"
    analysis-queue-routing-key = "mvn"

    entity-queue-name = "mvn-library-names"
    entity-queue-exchange-name = "library-name-exchange"
    entity-queue-routing-key = "mvn"
    entity-queue-max-priority = 3
}
```

Now you are ready to build all SPARRI components by executing `sbt clean docker` in the root directory of this repository. This command will compile and assemble all SPARRI components, and will also publish them as Docker images to your local Docker registry.

## Running SPARRI
You can execute the different components of SPARRI via Docker. You will need to host at least the WebAPI, EntityMiner and AnalysisRunner. This is done by executing

```
docker run -d --name sparri-api -p 9090:9090 spar-webapi:latest
docker run -d --name sparri-miner spar-maven-entity-miner:latest
docker run -d --name sparri-runner spar-analysis-runner:latest
```

Once all three containers are up and running, you can interact with SPARRI via the API hosted at port 9090. Refer to `./webapi/api_spec_openapi3.yaml` for a complete specification of our HTTP API.