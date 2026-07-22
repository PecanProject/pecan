# Shared CLI helpers for mslsp-extract entry points.

MSLSP_OW_TOKENS <- c("true", "t", "yes", "y", "overwrite")

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
  any(tolower(argv) %in% MSLSP_OW_TOKENS)
}

parse_cli_tile <- function(argv) {
  if (length(argv) < 2L) {
    return(NULL)
  }
  for (tok in argv[-1L]) {
    if (tolower(tok) %in% MSLSP_OW_TOKENS) next
    if (grepl("^[0-9][0-9A-Z]{4}$", tok)) {
      return(tok)
    }
  }
  NULL
}

argv_without_overwrite <- function(argv) {
  argv[!tolower(argv) %in% MSLSP_OW_TOKENS]
}
