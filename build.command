#!/bin/bash

# Exit immediately if any command fails
set -e

# Check if an argument is provided
if [ $# -eq 0 ]; then
  echo "Usage: $0 <filename>"
  exit 1
fi

input_file="$1"

# Check input file
if [ ! -f "$input_file" ]; then
  echo "Error: File '$input_file' not found."
  exit 1
fi

# Check SECRETS file
secrets_file="SECRETS"
if [ ! -f "$secrets_file" ]; then
  echo "Error: SECRETS file not found."
  exit 1
fi

# Read the first and second lines from SECRETS
id1_value=$(sed -n '1p' "$secrets_file")
id2_value=$(sed -n '2p' "$secrets_file")

# Check if the 'build' directory exists, if not, create it
build_dir="$(dirname "$input_file")/build"
if [ ! -d "$build_dir" ]; then
  echo "Build directory not found. Creating '$build_dir'."
  mkdir -p "$build_dir"
fi

# Create output filename in the 'build' directory
filename=$(basename "$input_file")
output_file="${build_dir}/builded__${filename}"

# Replace $ID1$ with id1_value and $ID2$ with id2_value
sed -e "s/\\\$ID1\\\$/$(printf '%s' "$id1_value")/g" \
    -e "s/\\\$ID2\\\$/$(printf '%s' "$id2_value")/g" \
    "$input_file" > "$output_file"

echo "Created: $output_file"
