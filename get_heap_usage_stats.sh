#!/bin/bash

# Check if log file path and output file path are provided
if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <path_to_gc_log> <output_file_path>"
    exit 1
fi

log_file="$1"
output_file="$2"

# Initialize variables for tracking memory usage
declare -a heap_usages=()
total_heap_usage=0
count=0

# Read the gc.log file line by line
while IFS= read -r line; do
    # Use regex to find lines with heap usage information (adjusted for better matching)
    if [[ $line =~ ([0-9]+)M-\> ]]; then
        pre_gc_usage="${BASH_REMATCH[1]}"
        heap_usages+=("$pre_gc_usage")
        total_heap_usage=$((total_heap_usage + pre_gc_usage))
        count=$((count + 1))
    fi
done < "$log_file"

# Calculate maximum and average
if [ $count -gt 0 ]; then
    max_heap_usage=$(printf "%s\n" "${heap_usages[@]}" | sort -n | tail -n 1)
    average_heap_usage=$((total_heap_usage / count))

    # Calculate variance and standard deviation without using bc
    sum_of_squares=0
    for usage in "${heap_usages[@]}"; do
        diff=$((usage - average_heap_usage))
        sum_of_squares=$((sum_of_squares + diff * diff))
    done

    variance=$((sum_of_squares / count))

    # Approximate square root of variance using integer arithmetic (simple method)
    std_deviation=0
    while (( std_deviation * std_deviation < variance )); do
        ((std_deviation++))
    done

else
    max_heap_usage=0
    average_heap_usage=0.00
    std_deviation=0.00
fi

# Print results to stdout
echo "Maximum Heap Usage: ${max_heap_usage} MB"
echo "Average Heap Usage: ${average_heap_usage} MB"
echo "Standard Deviation: ${std_deviation} MB"

# Write results to the specified output CSV file
echo "Maximum Heap Usage,Average Heap Usage,Standard Deviation" > "$output_file"
echo "${max_heap_usage},${average_heap_usage},${std_deviation}" >> "$output_file"

echo "Memory statistics have been written to $output_file"