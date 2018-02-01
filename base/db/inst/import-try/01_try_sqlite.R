# ---
# title: Example TRY import workflow
# author: Alexey Shiklomanov
# ---

# Create the TRY SQLite database
try_files <- "~/Projects/try/try-data/1829.txt"
sqlite_file <- "try.sqlite"

if (!file.exists(sqlite_file)) {
  try2sqlite(try_files, sqlite_file)
}

trydb <- DBI::dbConnect(RSQLite::SQLite(), sqlite_file)
bety <- DBI::dbConnect(
  RPostgres::Postgres(),
  dbname = "bety",
  host = "test-pecan.bu.edu",
  user = "bety",
  password = "bety"
)

# Add citations

# Add sites

# Add data
