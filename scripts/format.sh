#!/bin/bash

# Script Name: format_staged_r_files.sh
# Description: Formats only the staged .R files in the Git repository using the styler package in R.
#              This script is optimized for use as a pre-commit hook to reduce execution time.

# Exit immediately if a command exits with a non-zero status
set -e

# Function to check if an R package is installed
styler_in() {
  local pkg="$1"
  Rscript -e "if (!requireNamespace('$pkg', quietly = TRUE)) { quit(status=1) }"
}

# Check if styler is installed; install it if not
if styler_in "styler"; then
  echo "The 'styler' package is already installed."
else
  echo "The 'styler' package is not installed. Installing now..."
  Rscript -e "install.packages('styler', repos='https://cloud.r-project.org')"
  echo "'styler' package installed successfully."
fi

# Retrieve list of staged .R files
echo "Retrieving list of staged .R files..."
STAGED_FILES=$(git diff --cached --name-only --diff-filter=ACM | grep '\.R$' || true)

if [ -z "$STAGED_FILES" ]; then
  echo "No staged .R files to format."
  exit 0
fi

echo "Found the following staged .R files to format:"
echo "$STAGED_FILES"

# Iterate over each staged .R file and format it
for file in $STAGED_FILES; do
  if [ -f "$file" ]; then
    echo "Formatting: $file"
    # Format the file using styler
    Rscript -e "styler::style_file('$file')"

    # Check if the file was modified by styler
    if git diff --quiet "$file"; then
      echo "No changes made to: $file"
    else
      echo "Changes made to: $file. Re-adding to staging area."
      git add "$file"
    fi
  else
    echo "File not found: $file. Skipping."
  fi
done

echo "Staged .R files have been formatted successfully."
