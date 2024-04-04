#!/bin/bash

# Array of file names
file_names=("ment_health_age_grouped_bar" "phys_health_age_grouped_bar" "phys_health_ment_age_grouped_scatter", "3d_scatter_plot_Ment_Phys_Age")

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
