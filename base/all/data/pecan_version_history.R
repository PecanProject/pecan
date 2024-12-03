
# Read and format a list of pecan versions

# The local() wrapper is to avoid adding objects to the package data:
# Any extra vars defined at the top level of this file would be loaded
# into the global environment by `data("pecan_version_history")`

pecan_version_history <- local({
  pvh <- utils::read.csv(
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

  for (col in colnames(pvh)) {
    if (col != "package") {
      pvh[[col]] <- package_version(
        pvh[[col]],
        strict = strict)
    }
  }

  pvh
})
