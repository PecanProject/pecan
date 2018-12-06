# Character vector containing TRY file paths
try_files <- c("~/Projects/try-data/4143.txt")
stopifnot(all(file.exists(try_files)))

# Path to generated TRY SQLite file
sqlite_file <- "inst/import-try/try.sqlite"

# Bety connection configuration
betyparams <- list(
  host = "localhost",
  dbname = "bety",
  user = "bety",
  password = "bety"
)
