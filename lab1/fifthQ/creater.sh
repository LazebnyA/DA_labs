#!/bin/bash

# Array of file names
file_names=("phys_health_ment_scatter" "phys_health_ment_bar" "ment_phys_health_bar", "phys_health_ment_grouped_bar", "ment_phys_health_grouped_bar", "ment_phys_health_boxplot", "phys_health_ment_boxplot")

# Check if starter.r exists
if [ ! -f "starter.r" ]; then
    echo "Error: starter.r not found."
    exit 1
fi

# Loop through each file name in the array
for name in "${file_names[@]}"; do
    # Create the file with .r extension
    touch "$name.r"
    echo "Created file: $name.r"

    # Append text from starter.r to the file
    cat starter.r >> "$name.r"
done
