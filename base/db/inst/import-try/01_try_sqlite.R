# Create the TRY SQLite database
library(PEcAn.DB)
library(PEcAn.logger)
library(here)

configfile <- here("inst", "import-try", "config.R")
source(configfile)

if (!exists("overwrite")) {
  overwrite <- FALSE
}

if (!file.exists(sqlite_file) || overwrite) {
  file.remove(sqlite_file)
  try2sqlite(try_files, sqlite_file)
} else {
  logger.info("TRY SQLite database already exists and `overwrite` is FALSE. ")
}
