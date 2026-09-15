# Shared CLI helpers for tillage/extract entry points.

NDTI_OW_TOKENS <- c("true", "t", "yes", "y", "overwrite")

parse_cli_year <- function(argv) {
  if (!length(argv)) {
    stop("Usage: ... <year> [tile_id] [overwrite]")
  }
  y <- suppressWarnings(as.integer(argv[1L]))
  if (is.na(y) || y < 1990L || y > 2100L) {
    stop("Invalid year: ", argv[1L])
  }
  y
}

parse_cli_overwrite <- function(argv) {
  any(tolower(argv) %in% NDTI_OW_TOKENS)
}

parse_cli_tile <- function(argv) {
  if (length(argv) < 2L) {
    return(NULL)
  }
  for (tok in argv[-1L]) {
    if (tolower(tok) %in% NDTI_OW_TOKENS) {
      next
    }
    if (grepl("^[0-9][0-9A-Z]{4}$", tok)) {
      return(tok)
    }
  }
  NULL
}

# Optional month (1-12). Used when rerunning a single month.
parse_cli_month_optional <- function(argv) {
  if (length(argv) < 2L) {
    return(NULL)
  }
  for (tok in argv[-1L]) {
    if (tolower(tok) %in% NDTI_OW_TOKENS) {
      next
    }
    if (grepl("^[0-9][0-9A-Z]{4}$", tok)) {
      next
    }
    m <- suppressWarnings(as.integer(tok))
    if (!is.na(m) && m >= 1L && m <= 12L) {
      return(m)
    }
  }
  NULL
}

parse_cli_month <- function(argv) {
  m <- parse_cli_month_optional(argv)
  if (is.null(m)) {
    stop("Usage: ... <year> <month 1-12> [overwrite]")
  }
  m
}
