#!/bin/bash

# === Configuration ===

# Set the root directory to the current working directory, which is the PEcAN repository
ROOT_DIRECTORY=$(pwd)

# Define the path to the tracking CSV file that will log the formatting history of R files
# This CSV file will help keep track of when each file was last formatted and its MD5 hash
TRACKING_CSV_FILE="$ROOT_DIRECTORY/scripts/formatted.csv"

# Locate the Rscript command to be used for formatting R files
R_SCRIPT_COMMAND=$(which Rscript)

if [ -z "$R_SCRIPT_COMMAND" ]; then
    echo "Rscript not found. Please ensure R is installed and Rscript is in your PATH."
    exit 1
fi

# === Initialize Tracking File ===

# Check if the tracking CSV file exists; if not, create it and add the header
if [ ! -f "$TRACKING_CSV_FILE" ]; then
    echo "File_Path,MD5_Hash,Last_Formatted" > "$TRACKING_CSV_FILE"
    echo "Initialized tracking file at $TRACKING_CSV_FILE"
fi

# === Function to Compute MD5 Hash ===

# This function computes the MD5 hash of a given file
compute_md5_hash() {
    local file_path="$1"
    if [[ "$OSTYPE" == "darwin"* ]]; then
        # For macOS, use the md5 command
        md5 -q "$file_path"
    else
        # For Linux and other systems, use md5sum
        md5sum "$file_path" | awk '{ print $1 }'
    fi
}

# === Function to Format R File ===

# This function formats an R file using the styler package
format_r_file() {
    local file_path="$1"
    echo "Formatting $file_path..."
    $R_SCRIPT_COMMAND -e "styler::style_file('$file_path')"
}

# === Process Each R File ===

# Find all .R files recursively in the root directory
find "$ROOT_DIRECTORY" -type f -name "*.R" | while read -r R_FILE; do
    # Compute the relative path of the R file
    RELATIVE_PATH="${R_FILE#$ROOT_DIRECTORY/}"
    
    # Compute the current MD5 hash of the R file
    CURRENT_FILE_HASH=$(compute_md5_hash "$R_FILE")
    
    # Check if the file is already tracked in the CSV
    TRACKED_ENTRY=$(awk -F, -v path="$RELATIVE_PATH" '$1 == path { print $0 }' "$TRACKING_CSV_FILE")
    
    if [ -z "$TRACKED_ENTRY" ]; then
        # If the file is not tracked, format it and add a new entry to the CSV
        echo "New file detected: $RELATIVE_PATH. Formatting..."
        format_r_file "$R_FILE"
        NEW_FILE_HASH=$(compute_md5_hash "$R_FILE")
        TIMESTAMP=$(date +"%Y-%m-%d %H:%M:%S")
        echo "$RELATIVE_PATH,$NEW_FILE_HASH,$TIMESTAMP" >> "$TRACKING_CSV_FILE"
        echo "Added $RELATIVE_PATH to tracking file."
    else
        # Extract the stored hash from the tracked entry
        STORED_FILE_HASH=$(echo "$TRACKED_ENTRY" | awk -F, '{ print $2 }')
        
        if [ "$CURRENT_FILE_HASH" != "$STORED_FILE_HASH" ]; then
            # If the file has been modified since the last formatting, re-format it
            echo "Modified file detected: $RELATIVE_PATH. Re-formatting..."
            format_r_file "$R_FILE"
            UPDATED_FILE_HASH=$(compute_md5_hash "$R_FILE")
            UPDATED_TIMESTAMP=$(date +"%Y-%m-%d %H:%M:%S")
            # Update the CSV entry with the new hash and timestamp
            awk -F, -v path="$RELATIVE_PATH" -v new_hash="$UPDATED_FILE_HASH" -v new_time="$UPDATED_TIMESTAMP" '
                BEGIN { OFS = FS }
                $1 == path { $2 = new_hash; $3 = new_time }
                { print }
            ' "$TRACKING_CSV_FILE" > "${TRACKING_CSV_FILE}.tmp" && mv "${TRACKING_CSV_FILE}.tmp" "$TRACKING_CSV_FILE"
            echo "Updated $RELATIVE_PATH in tracking file."
        else
            # If the file has not been modified, skip formatting
            echo "No changes detected in $RELATIVE_PATH. Skipping formatting."
        fi
    fi
done

# === Roxygenize Documentation ===

# Run the documentation generation process using 'make document'
echo "Roxygenizing documentation with 'make document'..."
cd "$ROOT_DIRECTORY" || { echo "Failed to navigate to $ROOT_DIRECTORY"; exit 1; }
make document

echo "Formatting and documentation update completed."