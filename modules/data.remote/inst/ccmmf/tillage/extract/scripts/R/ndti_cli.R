# Shared CLI helpers for tillage/extract entry points.

parse_cli_year <- function(argv) {
  if (!length(argv)) {
    stop("Usage: ... <year> <month> [overwrite]")
  }
  y <- suppressWarnings(as.integer(argv[1L]))
  if (is.na(y) || y < 1990L || y > 2100L) {
    stop("Invalid year: ", argv[1L])
  }
  y
}

parse_cli_month <- function(argv) {
  if (length(argv) < 2L) {
    stop("Usage: ... <year> <month 1-12> [overwrite]")
  }
  m <- suppressWarnings(as.integer(argv[2L]))
  if (is.na(m) || m < 1L || m > 12L) {
    stop("Month must be 1-12: ", argv[2L])
  }
  m
}

parse_cli_overwrite <- function(argv) {
  any(tolower(argv) %in% c("true", "t", "yes", "y", "overwrite"))
}
