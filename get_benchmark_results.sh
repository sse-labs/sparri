#!/bin/bash

# Check if the correct number of arguments are provided
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <path_to_benchmark_directory> <output_csv_file>"
    exit 1
fi

# Assign arguments to variables
BENCHMARK_DIR="$1"
OUTPUT_CSV_FILE="$2"

# Create or clear the output CSV file and add header row
echo "Project Name,Analysis Type,Exec Time Run 0,Exec Time Run 1,Exec Time Run 2,Max Heap Run 0,Max Heap Run 1,Max Heap Run 2,Avg Heap Run 0,Avg Heap Run 1,Avg Heap Run 2" > "$OUTPUT_CSV_FILE"

# Iterate over each sub-directory (project directory) in the benchmark directory
for PROJECT_DIR in "$BENCHMARK_DIR"/*; do
    # Check if it's a directory
    if [ -d "$PROJECT_DIR" ]; then

        # Extract project name from project directory path
        PROJECT_NAME=$(basename "$PROJECT_DIR")

        # Initialize arrays to hold execution times and heap usage statistics
        EXEC_TIMES_WPA=()
        EXEC_TIMES_MOD=()
        MAX_HEAP_WPA=()
        MAX_HEAP_MOD=()
        AVG_HEAP_WPA=()
        AVG_HEAP_MOD=()

        # Iterate through run numbers (0 to 2)
        for RUN_NUMBER in {0..2}; do

            # Define paths for exec_time.log and mem_stats.log for WPA and MOD analyses
            WPA_RESULT_DIR="$PROJECT_DIR/lisi-eval/WPA-$RUN_NUMBER"
            MOD_RESULT_DIR="$PROJECT_DIR/lisi-eval/MOD-$RUN_NUMBER"

            # Collect data from WPA results if they exist
            if [ -f "$WPA_RESULT_DIR/exec_time.log" ]; then
                EXEC_TIME=$(cat "$WPA_RESULT_DIR/exec_time.log")
                EXEC_TIMES_WPA+=("$EXEC_TIME")

                MEM_STATS=$(cat "$WPA_RESULT_DIR/mem_stats.log")
                MAX_HEAP=$(echo "$MEM_STATS" | awk -F',' 'NR==2 {print $1}')
                AVG_HEAP=$(echo "$MEM_STATS" | awk -F',' 'NR==2 {print $2}')

                MAX_HEAP_WPA+=("$MAX_HEAP")
                AVG_HEAP_WPA+=("$AVG_HEAP")
            else
                EXEC_TIMES_WPA+=("N/A")
                MAX_HEAP_WPA+=("N/A")
                AVG_HEAP_WPA+=("N/A")
            fi

            # Collect data from MOD results if they exist
            if [ -f "$MOD_RESULT_DIR/exec_time.log" ]; then
                EXEC_TIME=$(cat "$MOD_RESULT_DIR/exec_time.log")
                EXEC_TIMES_MOD+=("$EXEC_TIME")

                MEM_STATS=$(cat "$MOD_RESULT_DIR/mem_stats.log")
                MAX_HEAP=$(echo "$MEM_STATS" | awk -F',' 'NR==2 {print $1}')
                AVG_HEAP=$(echo "$MEM_STATS" | awk -F',' 'NR==2 {print $2}')

                MAX_HEAP_MOD+=("$MAX_HEAP")
                AVG_HEAP_MOD+=("$AVG_HEAP")
            else
                EXEC_TIMES_MOD+=("N/A")
                MAX_HEAP_MOD+=("N/A")
                AVG_HEAP_MOD+=("N/A")
            fi

        done

        # Write collected data into the CSV file for both analysis types (WPA and MOD)
        echo "$PROJECT_NAME,WPA,${EXEC_TIMES_WPA[0]},${EXEC_TIMES_WPA[1]},${EXEC_TIMES_WPA[2]},${MAX_HEAP_WPA[0]},${MAX_HEAP_WPA[1]},${MAX_HEAP_WPA[2]},${AVG_HEAP_WPA[0]},${AVG_HEAP_WPA[1]},${AVG_HEAP_WPA[2]}" >> "$OUTPUT_CSV_FILE"

        echo "$PROJECT_NAME,MOD,${EXEC_TIMES_MOD[0]},${EXEC_TIMES_MOD[1]},${EXEC_TIMES_MOD[2]},${MAX_HEAP_MOD[0]},${MAX_HEAP_MOD[1]},${MAX_HEAP_MOD[2]},${AVG_HEAP_MOD[0]},${AVG_HEAP_MOD[1]}, ${AVG_HEAP_MOD[2]}" >> "$OUTPUT_CSV_FILE"

    fi
done

echo "Data collection complete. Results saved to '$OUTPUT_CSV_FILE'."
