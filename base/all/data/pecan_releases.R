
pecan_releases <- utils::read.csv(
  "pecan_releases.csv",
  colClasses = c(tag = "character", date = "Date", version = "character"))

pecan_releases$version <- package_version(pecan_releases$version)

rownames(pecan_releases) <- pecan_releases$tag
