#!/usr/bin/env Rscript
# NDTI tilewise driver: prep-static, extract, combine, or all for one year and month(s).
# Uses tilewise_core and NDTI implementation; writes under tillage/ndti_v4.1.
#
# Main inputs: year, command, month integers 1-12, optional overwrite.
# Main outputs: tilepiece CSV.gz and ndti_year=Y_month=MM.parquet per month.
# How to run: Rscript tilewise_ndti_driver.R <year> <command> [months ...] [overwrite]
# Workflow: monitoring workflow NDTI branch (see monitoring_workflow_flowchart.mmd).

script_dir <- if (length(file_arg <- commandArgs(trailingOnly = FALSE)[grepl("^--file=",
                   commandArgs(trailingOnly = FALSE))])) {
  dirname(sub("^--file=", "", file_arg[1]))
} else "."

suppressPackageStartupMessages(library(arrow))
local({
  tmp <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp))
  arrow::write_parquet(data.frame(x = 1L), tmp)
  arrow::read_parquet(tmp)
})

source(file.path(script_dir, "tilewise_ndti_implementation.R"))
source(file.path(script_dir, "combine_ndti_tilepieces.R"))
source(file.path(script_dir, "tilewise_core.R"))

path_ndti_output <- file.path(
  Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management"),
  "tillage/ndti_v4.1"
)
product <- product_ndti()

args <- commandArgs(trailingOnly = TRUE)
is_overwrite   <- function(x) tolower(x) %in% c("true", "t", "yes", "y", "overwrite")
overwrite_flag <- any(sapply(args, is_overwrite))
args           <- args[!sapply(args, is_overwrite)]

if (length(args) < 2) {
  stop("Usage: Rscript tilewise_ndti_driver.R <year> <prep-static|extract|combine|all> [month ...] [overwrite]")
}

year_arg <- as.integer(args[1])
command  <- tolower(args[2])

months_from <- function(x) {
  vals <- suppressWarnings(as.integer(x))
  unique(sort(vals[!is.na(vals) & vals >= 1 & vals <= 12]))
}

months_str <- paste(months_from(args[-(1:2)]), collapse = "-")
if (!nzchar(months_str)) months_str <- "prep"
ts      <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_dir <- file.path(path_ndti_output, sprintf("year=%d", year_arg), "logs")
tilewise_log_init(file.path(log_dir,
  sprintf("ndti_%s_m%s_%s.log", command, months_str, ts)))

tw_log("INFO", "NDTI driver  year=", year_arg, " command=", command,
       " overwrite=", overwrite_flag,
       " pid=", Sys.getpid(),
       " SGE_TASK_ID=", Sys.getenv("SGE_TASK_ID", ""))

if (command == "prep-static") {
  tilewise_prep_static(year_arg, product, overwrite = overwrite_flag)

} else if (command == "extract") {
  months <- months_from(args[-(1:2)])
  if (length(months) == 0) stop("Usage: ... extract <month> [month ...] [overwrite]")
  prep <- tilewise_prep_static(year_arg, product, overwrite = overwrite_flag)
  for (m in months) tilewise_run(prep, m, product, overwrite = overwrite_flag)

} else if (command == "combine") {
  months <- months_from(args[-(1:2)])
  if (length(months) == 0) stop("Usage: ... combine <month> [month ...] [overwrite]")
  prep <- tilewise_prep_static(year_arg, product)
  for (m in months) tilewise_combine(prep, m, product, overwrite = overwrite_flag)

} else if (command == "all") {
  months <- months_from(args[-(1:2)])
  if (length(months) == 0) stop("Usage: ... all <month> [month ...] [overwrite]")
  prep <- tilewise_prep_static(year_arg, product, overwrite = overwrite_flag)
  for (m in months) {
    tilewise_run(prep, m, product, overwrite = overwrite_flag)
    tilewise_combine(prep, m, product, overwrite = overwrite_flag)
  }

} else {
  stop("Unknown command: ", command, ". Use: prep-static | extract | combine | all")
}
