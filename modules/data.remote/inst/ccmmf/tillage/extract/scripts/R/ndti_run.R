# NDTI pipeline steps -- called by atomic CLI scripts and run_ndti.sh.

NDTI_MONTHS <- 1L:12L

ndti_init_arrow <- function() {
  suppressPackageStartupMessages(library(arrow))
  tmp <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp), add = TRUE)
  arrow::write_parquet(data.frame(x = 1L), tmp)
  arrow::read_parquet(tmp)
}

ndti_product <- function() {
  product_ndti()
}

# CLI tile, else TILEWISE_ONE_TILE, else DEMO_TILE. Statewide if none of those.
ndti_run_tile <- function(tile = NULL) {
  raw <- c(
    if (!is.null(tile) && length(tile) > 0L) trimws(as.character(tile)[1L]) else "",
    trimws(Sys.getenv("TILEWISE_ONE_TILE", "")),
    trimws(Sys.getenv("DEMO_TILE", ""))
  )
  skip <- c("", "1", "true", "yes", "y", "first")
  for (t in raw) {
    if (is.na(t) || !nzchar(t) || tolower(t) %in% skip) {
      next
    }
    if (grepl("^[0-9][0-9A-Z]{4}$", t)) {
      return(t)
    }
  }
  NULL
}

ndti_with_one_tile <- function(tile, fn) {
  tile <- if (is.null(tile)) "" else as.character(tile)[1L]
  old_tile <- Sys.getenv("TILEWISE_ONE_TILE", "")
  on.exit({
    if (nzchar(old_tile)) {
      Sys.setenv(TILEWISE_ONE_TILE = old_tile)
    } else {
      Sys.unsetenv("TILEWISE_ONE_TILE")
    }
  }, add = TRUE)
  if (nzchar(tile)) {
    Sys.setenv(TILEWISE_ONE_TILE = tile)
  }
  fn()
}

ndti_log_init <- function(year, command, month = NULL, tile = "") {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_dir <- file.path(path_ndti_out_root(), sprintf("year=%d", year), "logs")
  if (exists("tilewise_log_init", mode = "function")) {
    mpart <- if (!is.null(month)) sprintf("_m%02d", as.integer(month)) else ""
    tpart <- if (nzchar(tile)) sprintf("_%s", tile) else ""
    tilewise_log_init(file.path(log_dir,
      sprintf("ndti_%s%s%s_%s.log", command, mpart, tpart, ts)))
  }
}

ndti_month_jobs <- function() {
  j <- suppressWarnings(as.integer(Sys.getenv("NDTI_MONTH_JOBS", "1")))
  if (is.na(j) || j < 1L) 1L else j
}

ndti_buffer_days <- function() {
  b <- suppressWarnings(as.integer(Sys.getenv("HLS_DOWNLOAD_BUFFER_DAYS", "185")))
  if (is.na(b) || b < 0L) 185L else b
}

# Year-level extract/combine (month omitted) includes the 185-day forward
# shoulder after Dec 31 (into Y+1). Single-month reruns do not.
# NDTI_INCLUDE_LOOKAHEAD=0 or NDTI_INCLUDE_LOOKBACK=0 disables the shoulder.
ndti_include_lookback <- function() {
  raw <- Sys.getenv("NDTI_INCLUDE_LOOKAHEAD", "")
  if (!nzchar(trimws(raw))) {
    raw <- Sys.getenv("NDTI_INCLUDE_LOOKBACK", "true")
  }
  v <- tolower(trimws(raw))
  !v %in% c("0", "false", "no", "n", "off")
}

# Months overlapping Jan 1 Y through Dec 31 Y plus HLS_DOWNLOAD_BUFFER_DAYS.
ndti_window_month_pairs <- function(year) {
  year <- as.integer(year)
  start <- as.Date(sprintf("%d-01-01", year))
  end <- as.Date(sprintf("%d-12-31", year)) + ndti_buffer_days()
  y <- as.integer(format(start, "%Y"))
  m <- as.integer(format(start, "%m"))
  y1 <- as.integer(format(end, "%Y"))
  m1 <- as.integer(format(end, "%m"))
  out <- list()
  repeat {
    out[[length(out) + 1L]] <- list(year = y, month = m)
    if (y == y1 && m == m1) {
      break
    }
    m <- m + 1L
    if (m > 12L) {
      m <- 1L
      y <- y + 1L
    }
  }
  out
}

ndti_extract_pairs <- function(year, month = NULL) {
  year <- as.integer(year)
  if (!is.null(month)) {
    return(list(list(year = year, month = as.integer(month))))
  }
  if (!ndti_include_lookback() || ndti_buffer_days() <= 0L) {
    return(lapply(NDTI_MONTHS, function(m) list(year = year, month = as.integer(m))))
  }
  ndti_window_month_pairs(year)
}

ndti_pairs_label <- function(pairs) {
  paste(
    vapply(pairs, function(p) sprintf("%d-%02d", p$year, p$month), character(1)),
    collapse = ","
  )
}

# Forward hive months (Y+1) must extract the job year's parcels, not Y+1 LandIQ.
ndti_ensure_parcel_years <- function(job_year) {
  if (!nzchar(trimws(Sys.getenv("NDTI_PARCEL_YEARS", "")))) {
    Sys.setenv(NDTI_PARCEL_YEARS = as.character(as.integer(job_year)))
  }
}

# Separate R processes per (year, month) (terra is not fork-safe). Children
# set NDTI_MONTH_JOBS=1 and pass month so they do not expand the shoulder again.
run_ndti_extract_month_processes <- function(pairs, overwrite, tile, jobs) {
  script <- file.path(ndti_extract_pkg_root(), "scripts", "extract_tiles.R")
  if (!file.exists(script)) {
    stop("NDTI extract_tiles.R not found: ", script)
  }
  if (!nzchar(trimws(Sys.getenv("NDTI_TERRA_THREADS", "")))) {
    Sys.setenv(NDTI_TERRA_THREADS = "1")
  }
  extra <- character()
  if (nzchar(tile)) extra <- c(extra, tile)
  if (isTRUE(overwrite)) extra <- c(extra, "overwrite")
  extra_q <- if (length(extra)) paste(shQuote(extra), collapse = " ") else ""
  specs <- vapply(
    pairs,
    function(p) sprintf("%d,%d", as.integer(p$year), as.integer(p$month)),
    character(1)
  )
  spec_q <- paste(shQuote(specs), collapse = " ")
  inner <- paste(
    "y=\"${1%%,*}\"; m=\"${1#*,}\";",
    "exec Rscript", shQuote(script), "\"$y\" \"$m\"", extra_q
  )
  if (exists("tw_log", mode = "function")) {
    tw_log("INFO", "NDTI extract month jobs=", jobs, " n=", length(pairs),
           " script=", script)
  }
  cmd <- paste(
    "printf '%s\\n'", spec_q, "|",
    "NDTI_MONTH_JOBS=1",
    "xargs -P", as.integer(jobs), "-I{}",
    "sh -c", shQuote(inner),
    "_",
    "{}"
  )
  status <- system(cmd)
  if (!identical(as.integer(status), 0L)) {
    stop("NDTI parallel month extract failed (status=", status, ")")
  }
  invisible(NULL)
}

ndti_get_prep <- function(year, overwrite = FALSE, tile = NULL) {
  tilewise_prep_static(year, ndti_product(), overwrite = overwrite, tile = tile)
}

run_ndti_prep_static <- function(year) {
  ndti_init_arrow()
  year <- as.integer(year)
  ndti_log_init(year, "prep-static")
  if (exists("tw_log", mode = "function")) {
    tw_log("INFO", "NDTI prep-static year=", year, " pid=", Sys.getpid())
  }
  ndti_get_prep(year)
}

# Extract one year or a single month. Year-level (month omitted) also writes
# the HLS_DOWNLOAD_BUFFER_DAYS forward months after Dec 31 (hive year=Y+1).
# Prep loads once per calendar year in the pair list.
run_ndti_extract <- function(year, month = NULL, overwrite = FALSE, tile = NULL) {
  ndti_init_arrow()
  year <- as.integer(year)
  if (is.null(month)) {
    ndti_ensure_parcel_years(year)
  }
  pairs <- ndti_extract_pairs(year, month)
  tile <- ndti_run_tile(tile)
  tile_s <- if (is.null(tile)) "" else tile
  ndti_log_init(year, "extract",
                month = if (length(pairs) == 1L) pairs[[1L]]$month else NULL,
                tile = tile_s)
  if (exists("tw_log", mode = "function")) {
    tw_log("INFO", "NDTI extract job_year=", year,
           " months=", ndti_pairs_label(pairs),
           if (nzchar(tile_s)) paste0(" tile=", tile_s) else "",
           " overwrite=", overwrite,
           " jobs=", ndti_month_jobs(),
           " pid=", Sys.getpid())
  }

  ndti_with_one_tile(tile_s, function() {
    jobs <- ndti_month_jobs()
    if (jobs > 1L && length(pairs) > 1L) {
      run_ndti_extract_month_processes(pairs, overwrite, tile_s, jobs)
      return(invisible(NULL))
    }
    cal_years <- unique(vapply(pairs, function(p) as.integer(p$year), integer(1)))
    for (cy in cal_years) {
      months_y <- vapply(
        Filter(function(p) as.integer(p$year) == cy, pairs),
        function(p) as.integer(p$month),
        integer(1)
      )
      prep <- ndti_get_prep(
        cy,
        overwrite = overwrite,
        tile = if (nzchar(tile_s)) tile_s else NULL
      )
      if (nzchar(tile_s) && !tile_s %in% names(prep$tile_to_parcel_ids)) {
        msg <- paste0("tile=", tile_s, " no ag parcels for year=", cy, " -- skip")
        if (exists("tw_log", mode = "function")) {
          tw_log("INFO", msg)
        } else {
          message("[NDTI] ", msg)
        }
        next
      }
      for (m in months_y) {
        if (exists("tw_log", mode = "function")) {
          tw_log("INFO", "NDTI extract year=", cy, " month=", m)
        }
        tilewise_run(prep, m, ndti_product(), overwrite = overwrite)
        terra::tmpFiles(orphan = TRUE, remove = TRUE)
        invisible(gc(verbose = FALSE))
      }
    }
  })
}

# Combine one year or a single month. Year-level also combines forward months.
run_ndti_combine <- function(year, month = NULL, overwrite = FALSE, tile = NULL) {
  ndti_init_arrow()
  year <- as.integer(year)
  if (is.null(month)) {
    ndti_ensure_parcel_years(year)
  }
  pairs <- ndti_extract_pairs(year, month)
  tile <- ndti_run_tile(tile)
  tile_s <- if (is.null(tile)) "" else tile
  ndti_log_init(year, "combine",
                month = if (length(pairs) == 1L) pairs[[1L]]$month else NULL,
                tile = tile_s)
  if (exists("tw_log", mode = "function")) {
    tw_log("INFO", "NDTI combine job_year=", year,
           " months=", ndti_pairs_label(pairs),
           if (nzchar(tile_s)) paste0(" tile=", tile_s) else "",
           " overwrite=", overwrite)
  }

  ndti_with_one_tile(tile_s, function() {
    cal_years <- unique(vapply(pairs, function(p) as.integer(p$year), integer(1)))
    for (cy in cal_years) {
      months_y <- vapply(
        Filter(function(p) as.integer(p$year) == cy, pairs),
        function(p) as.integer(p$month),
        integer(1)
      )
      prep <- ndti_get_prep(
        cy,
        overwrite = FALSE,
        tile = if (nzchar(tile_s)) tile_s else NULL
      )
      for (m in months_y) {
        if (exists("tw_log", mode = "function")) {
          tw_log("INFO", "NDTI combine year=", cy, " month=", m)
        }
        tilewise_combine(prep, m, ndti_product(), overwrite = overwrite)
      }
    }
  })
}

run_ndti_all <- function(year, overwrite = FALSE, tile = NULL) {
  run_ndti_extract(year, overwrite = overwrite, tile = tile)
  run_ndti_combine(year, overwrite = overwrite, tile = tile)
}
