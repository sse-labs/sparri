#!/bin/bash

# Check if an argument is supplied
if [ -z "$1" ]; then
  echo "Usage: $0 <path_to_maven_project>"
  exit 1
fi

PROJECT_PATH="$1"

# Execute the docker command with the supplied path
docker run -v "${PROJECT_PATH}:/app/input/" -v "${HOME}/.m2/:/root/.m2" --name sparri-wpa-taint lisi-evaluation /app/input
