# NDTI pipeline steps — called by atomic CLI scripts and run_ndti.sh.

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

ndti_log_init <- function(year, month, command) {
  ts <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_dir <- file.path(path_ndti_out_root(), sprintf("year=%d", year), "logs")
  if (exists("tilewise_log_init", mode = "function")) {
    tilewise_log_init(file.path(log_dir,
      sprintf("ndti_%s_m%02d_%s.log", command, as.integer(month), ts)))
  }
}

run_ndti_prep_static <- function(year) {
  ndti_init_arrow()
  year <- as.integer(year)
  ndti_log_init(year, 0L, "prep-static")
  if (exists("tw_log", mode = "function")) {
    tw_log("INFO", "NDTI prep-static year=", year, " pid=", Sys.getpid())
  }
  tilewise_prep_static(year, ndti_product())
}

run_ndti_extract <- function(year, month, overwrite = FALSE) {
  ndti_init_arrow()
  year  <- as.integer(year)
  month <- as.integer(month)
  ndti_log_init(year, month, "extract")
  if (exists("tw_log", mode = "function")) {
    tw_log("INFO", "NDTI extract year=", year, " month=", month,
           " overwrite=", overwrite, " pid=", Sys.getpid(),
           " TASK_ID=", Sys.getenv("TASK_ID", ""))
  }
  prep <- tilewise_prep_static(year, ndti_product())
  tilewise_run(prep, month, ndti_product(), overwrite = overwrite)
}

run_ndti_combine <- function(year, month, overwrite = FALSE) {
  ndti_init_arrow()
  year  <- as.integer(year)
  month <- as.integer(month)
  ndti_log_init(year, month, "combine")
  if (exists("tw_log", mode = "function")) {
    tw_log("INFO", "NDTI combine year=", year, " month=", month,
           " overwrite=", overwrite)
  }
  prep <- tilewise_prep_static(year, ndti_product())
  tilewise_combine(prep, month, ndti_product(), overwrite = overwrite)
}

run_ndti_all <- function(year, month, overwrite = FALSE) {
  run_ndti_extract(year, month, overwrite = overwrite)
  run_ndti_combine(year, month, overwrite = overwrite)
}
