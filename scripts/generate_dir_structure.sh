#!/bin/bash

# File: scripts/generate_dir_structure.sh
# Purpose: Generate a simple Markdown-style directory structure for documentation

OUTPUT_FILE="documentation/directory_structure.md"

# Create the output directory if it doesn't exist
mkdir -p documentation

# Write header
echo "# PEcAn Directory Structure" > "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"
echo "_This file is auto-generated. To update, run: \`make generate-dir-doc\`_" >> "$OUTPUT_FILE"
echo "" >> "$OUTPUT_FILE"

# List top-level directories and their immediate subdirectories (1-level deep)
for dir in */ ; do
  if [ -d "$dir" ]; then
    echo "- \`$dir\`" >> "$OUTPUT_FILE"
    for sub in "$dir"*/ ; do
      if [ -d "$sub" ]; then
        echo "  - \`${sub#$dir}\`" >> "$OUTPUT_FILE"
      fi
    done
  fi
done

echo "✅ Directory structure written to $OUTPUT_FILE"
