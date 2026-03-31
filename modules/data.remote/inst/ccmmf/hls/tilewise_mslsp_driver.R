#!/usr/bin/env Rscript
# MSLSP tilewise driver: prep-static, extract, combine, or all for one year.
# Uses tilewise_core plus MSLSP implementation and combine_mslsp_tilepieces.R.
#
# Main inputs: year, command prep-static|extract|combine|all, optional overwrite.
# Main outputs: tilepiece CSV.gz under phenology/raw_mslsp_v4.1, then mslsp_year=Y.parquet.
# How to run: Rscript tilewise_mslsp_driver.R <year> [prep-static|extract|combine|all] [overwrite]
# Workflow: monitoring workflow stage S1-S5 for MSLSP (see monitoring_workflow_flowchart.mmd).

script_dir <- if (length(file_arg <- commandArgs(trailingOnly = FALSE)[grepl("^--file=",
                   commandArgs(trailingOnly = FALSE))])) {
  dirname(sub("^--file=", "", file_arg[1]))
} else "."

# Arrow registers filesystem handlers lazily. Touch parquet before sf/terra/GDAL load.
suppressPackageStartupMessages(library(arrow))
local({
  tmp <- tempfile(fileext = ".parquet")
  on.exit(unlink(tmp))
  arrow::write_parquet(data.frame(x = 1L), tmp)
  arrow::read_parquet(tmp)
})

source(file.path(script_dir, "extract_summary_core.R"))
source(file.path(script_dir, "tilewise_core.R"))
source(file.path(script_dir, "tilewise_mslsp_implementation.R"))
source(file.path(script_dir, "combine_mslsp_tilepieces.R"))

product <- product_mslsp()

args <- commandArgs(trailingOnly = TRUE)
is_overwrite   <- function(x) tolower(x) %in% c("true", "t", "yes", "y", "overwrite")
overwrite_flag <- any(sapply(args, is_overwrite))
args           <- args[!sapply(args, is_overwrite)]

if (length(args) < 1) {
  stop("Usage: Rscript tilewise_mslsp_driver.R <year> [prep-static|extract|combine|all] [overwrite]")
}

year_arg   <- as.integer(args[1])
command    <- if (length(args) >= 2 && nzchar(args[2])) tolower(args[2]) else "all"
mslsp_time_key <- 1L

ts      <- format(Sys.time(), "%Y%m%d_%H%M%S")
log_dir <- file.path(mslsp_out_root, sprintf("year=%d", year_arg), "logs")
tilewise_log_init(file.path(log_dir,
  sprintf("mslsp_%s_%s.log", command, ts)))

tw_log("INFO", "MSLSP driver  year=", year_arg, " command=", command,
       " overwrite=", overwrite_flag,
       " pid=", Sys.getpid(),
       " SGE_TASK_ID=", Sys.getenv("SGE_TASK_ID", ""))

if (command == "prep-static") {
  tilewise_prep_static(year_arg, product)

} else if (command == "extract") {
  prep <- tilewise_prep_static(year_arg, product)
  tilewise_run(prep, mslsp_time_key, product, overwrite = overwrite_flag)

} else if (command == "combine") {
  prep <- tilewise_prep_static(year_arg, product)
  tilewise_combine(prep, mslsp_time_key, product, overwrite = overwrite_flag)

} else if (command == "all") {
  prep <- tilewise_prep_static(year_arg, product)
  tilewise_run(prep, mslsp_time_key, product, overwrite = overwrite_flag)
  tilewise_combine(prep, mslsp_time_key, product, overwrite = overwrite_flag)

} else {
  stop("Unknown command: ", command, ". Use: prep-static | extract | combine | all")
}
