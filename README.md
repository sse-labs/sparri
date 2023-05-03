# SPARRI
This repository holds a Scala-based implementation of the Static Program Analysis Result Reuse Index (SPARRI). In order to compile and run SPARRI, you will need:
- JDK 1.8
- SBT (Scala Build Tool)
- Docker

## Overview
SPARRI consists of multiple components, each of which is realized as a separate Scala module:
- **./analysis-runner** Component that executes built-in analyses on demand
- **./core** Core functionality shared among SPARRI components
- **./evaluation** Evaluation as reported in our paper
- **./maven-entity-miner** Component that indexes programs (GAV-Triples) from Maven Central on demand
- **./maven-entity-name-publisher** Component that requests indexing of all Maven Central programs
- **./webapi** Implementation of an RESTful HTTP API

Furthermore, this repository contains:
- **./playground** A Scala project for testing SPARRI functionality
- **./literature-review-result** Directory holding all intermediate and final results of our literature survey on SPA implementations

## Getting Started
