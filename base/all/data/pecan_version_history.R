
pecan_version_history <- utils::read.csv(
  "pecan_version_history.csv",
  colClasses = "character",
  check.names = FALSE)

for (col in colnames(pecan_version_history)) {
  if (col != "package") {
    pecan_version_history[[col]] <- package_version(pecan_version_history[[col]])
  }
}
