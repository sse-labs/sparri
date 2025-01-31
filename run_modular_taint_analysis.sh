#!/bin/bash

# Check if at least two arguments are supplied
if [ "$#" -lt 2 ]; then
  echo "Usage: $0 <path_to_maven_project> <output_path>"
  exit 1
fi

PROJECT_PATH="$1"
OUTPUT_PATH="$2"

# Check if the OUTPUT_PATH is a valid directory
if [ ! -d "$OUTPUT_PATH" ]; then
  echo "Error: Output path '$OUTPUT_PATH' is not a valid directory."
  exit 1
fi

START=$(date +%s)
docker run --rm -v "${PROJECT_PATH}:/app/input/" -v "${OUTPUT_PATH}:/app/stats" --name sparri-modular-taint spar-analyses /app/input /app/stats
END=$(date +%s)

# Extract real execution time in seconds
REAL_TIME=$((END-START))

# Write only the execution time (in seconds) to exec_time.log in the output path
echo "$REAL_TIME" > "${OUTPUT_PATH}/exec_time.log"

# Print the execution time in seconds to stdout
echo "Docker command executed in: $REAL_TIME seconds"

# Extract memory metrics from GC log
./get_heap_usage_stats.sh "${OUTPUT_PATH}/gc.log" "${OUTPUT_PATH}/mem_stats.log"

echo "Execution time has been logged to ${OUTPUT_PATH}/exec_time.log"