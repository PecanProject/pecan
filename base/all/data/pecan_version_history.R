

pecan_version_history <- utils::read.csv(
  "pecan_version_history.csv",
  colClasses = "character",
  check.names = FALSE)

# We'd like to parse strictly to catch invalid versions (probably typos).
# But we _need_ to allow NAs... and in R < 4.4, package_version did not
# accept NAs unless strict=FALSE.
strict <- TRUE
na_version <- try(
  package_version(NA_character_, strict = strict),
  silent = TRUE)
if (inherits(na_version, "try-error")) {
  strict <- FALSE
}

for (col in colnames(pecan_version_history)) {
  if (col != "package") {
    pecan_version_history[[col]] <- package_version(
      pecan_version_history[[col]],
      strict = strict)
  }
}
