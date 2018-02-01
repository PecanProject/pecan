# Create the TRY SQLite database
library(PEcAn.DB)

# Create the TRY SQLite database
try_files <- "~/try-data/1829.txt"
sqlite_file <- "try.sqlite"

if (!exists("overwrite")) {
  overwrite <- FALSE
}

if (!file.exists(sqlite_file) || overwrite) {
  try2sqlite(try_files, sqlite_file)
}

if (!exists("force_add_doi")) {
  force_add_doi <- FALSE
}
