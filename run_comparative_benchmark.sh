#!/bin/bash

# Check if the correct number of arguments are provided
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <path_to_benchmark_directory> <run_number>"
    exit 1
fi

# Assign arguments to variables
BENCHMARK_DIR="$1"
RUN_NUMBER="$2"

# Check if the benchmark directory exists
if [ ! -d "$BENCHMARK_DIR" ]; then
    echo "Error: Directory '$BENCHMARK_DIR' does not exist."
    exit 1
fi

# Check if run number is an integer
if ! [[ "$RUN_NUMBER" =~ ^[0-9]+$ ]]; then
    echo "Error: Run number '$RUN_NUMBER' is not a valid integer."
    exit 1
fi

# Iterate over each sub-directory (project directory) in the benchmark directory
for PROJECT_DIR in "$BENCHMARK_DIR"/*; do
    # Check if it's a directory
    if [ -d "$PROJECT_DIR" ]; then

        # Define the path for 'lisi-eval' result directory
        RESULT_DIR="$PROJECT_DIR/lisi-eval"

        # Create 'lisi-eval' result directory if it doesn't exist
        mkdir -p "$RESULT_DIR"

        # Create subdirectories WPA-<run_number> and MOD-<run_number>
        WPA_RESULT_DIR="$RESULT_DIR/WPA-$RUN_NUMBER"
        MOD_RESULT_DIR="$RESULT_DIR/MOD-$RUN_NUMBER"
        mkdir -p "$WPA_RESULT_DIR"
        mkdir -p "$MOD_RESULT_DIR"

        # Define analysis directory path based on .lisipath file existence
        LISIPATH_FILE="$PROJECT_DIR/.lisipath"

        if [ -f "$LISIPATH_FILE" ]; then
            RELATIVE_PATH=$(cat "$LISIPATH_FILE")
            ANALYSIS_DIR="$PROJECT_DIR/$RELATIVE_PATH"
        else
            ANALYSIS_DIR="$PROJECT_DIR"
        fi

        # Extract project name from project directory path for logging purposes
        PROJECT_NAME=$(basename "$PROJECT_DIR")

        # Invoke WPA taint analysis script with appropriate arguments and log message
        echo "Running WPA taint analysis on project: $PROJECT_NAME"
        ./run_wpa_taint_analysis.sh "$ANALYSIS_DIR" "$WPA_RESULT_DIR"

        # Invoke MOD taint analysis script with appropriate arguments and log message
        echo "Running modular taint analysis on project: $PROJECT_NAME"
        ./run_modular_taint_analysis.sh "$ANALYSIS_DIR" "$MOD_RESULT_DIR"

    fi
done

echo "Finished processing all project directories."
