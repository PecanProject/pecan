#!/usr/bin/env Rscript
# =====================================================================
# build_qc_report.R
# Build a QC report for LandIQ-MSLSP matching: narrative summary,
# tables by year (MULTIUSE, CLASS, SUBCLASS, season, assigned cycle,
# qc_adoy_vs_cycle, match_outcome), and "ways to improve / look into".
#
# Usage:
#   Rscript scripts/phenology/build_qc_report.R
#   Rscript -e "REPORT_DIR='/path/to/out'; source('scripts/phenology/build_qc_report.R')"
#
# Reads: matched_landiq_mslsp_v4.1.2/assigned_year=Y.parquet (all available years)
# Writes: matched_landiq_mslsp_v4.1.2/QC_report_YYYYMMDD.md (and optional CSV tables)
# =====================================================================

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "")
if (!nzchar(trimws(path_management))) {
  .root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
  if (!nzchar(.root)) {
    stop("Set CCMMF_MANAGEMENT or CCMMF_ROOT (source documentation/setup_env.sh).")
  }
  path_management <- file.path(.root, "management")
}
.path_code <- trimws(Sys.getenv("CCMMF_CODE", ""))
.script_dir <- tryCatch(
  dirname(normalizePath(sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE)[1L], mustWork = FALSE)),
  error = function(e) getwd()
)
.matched_candidates <- c(
  if (nzchar(.path_code)) file.path(.path_code, "phenology", "match", "matched_paths.R") else character(),
  file.path(.script_dir, "matched_paths.R"),
  file.path(.script_dir, "..", "match", "matched_paths.R"),
  file.path(path_management, "scripts", "phenology", "matched_paths.R")
)
.matched_paths <- .matched_candidates[file.exists(.matched_candidates)][1L]
if (is.na(.matched_paths) || !nzchar(.matched_paths)) {
  stop("Could not find matched_paths.R (set CCMMF_CODE or place next to this script).")
}
source(.matched_paths)
matched_dir    <- matched_landiq_dir(path_management)
report_dir     <- if (exists("REPORT_DIR", envir = .GlobalEnv)) get("REPORT_DIR", envir = .GlobalEnv) else matched_dir
report_years   <- if (exists("REPORT_YEARS", envir = .GlobalEnv)) get("REPORT_YEARS", envir = .GlobalEnv) else NULL

# Discover assigned parquet files
assigned_files <- list.files(matched_dir, pattern = "^assigned_year=[0-9]+\\.parquet$", full.names = TRUE)
if (length(assigned_files) == 0) stop("No assigned_year=*.parquet found in ", matched_dir)
years_from_files <- as.integer(sub("^assigned_year=([0-9]+)\\.parquet$", "\\1", basename(assigned_files)))
years <- sort(unique(years_from_files))
if (length(report_years) > 0) years <- intersect(years, as.integer(report_years))
if (length(years) == 0) stop("No years to report")

message("Building QC report for years: ", paste(years, collapse = ", "))

# Load all assigned data
assign_list <- setNames(vector("list", length(years)), years)
for (yr in years) {
  path <- file.path(matched_dir, paste0("assigned_year=", yr, ".parquet"))
  if (!file.exists(path)) next
  assign_list[[as.character(yr)]] <- as.data.table(read_parquet(path))
}
assign_list <- assign_list[!sapply(assign_list, is.null)]
if (length(assign_list) == 0) stop("No assigned data loaded")

# Normalize column names (MULTIUSE may be missing in older runs)
for (i in seq_along(assign_list)) {
  d <- assign_list[[i]]
  if (!"landiq_MULTIUSE" %in% names(d)) d[, landiq_MULTIUSE := NA_character_]
  d[, landiq_MULTIUSE := trimws(as.character(landiq_MULTIUSE))]
  d[is.na(landiq_MULTIUSE) | landiq_MULTIUSE == "", landiq_MULTIUSE := "(blank)"]
  assign_list[[i]] <- d
}

# Field-year rollup: one row per parcel-year (avoid symbol 'year' in by - can be masked)
fy_rollup <- function(dt) {
  dt <- as.data.table(dt)
  bycols <- c("parcel_id", "year")
  keys <- unique(dt[, .SD, .SDcols = bycols])
  setkeyv(keys, bycols)
  mo <- dt[!is.na(match_outcome), .(match_outcome = match_outcome[1L]), keyby = bycols]
  pft <- dt[!is.na(landiq_PFT) & nzchar(trimws(landiq_PFT)), .(landiq_PFT = landiq_PFT[1L]), keyby = bycols]
  cl  <- dt[!is.na(landiq_CLASS) & nzchar(trimws(landiq_CLASS)), .(landiq_CLASS = landiq_CLASS[1L]), keyby = bycols]
  sub <- dt[!is.na(landiq_SUBCLASS) & nzchar(trimws(landiq_SUBCLASS)), .(landiq_SUBCLASS = landiq_SUBCLASS[1L]), keyby = bycols]
  mu  <- dt[!is.na(landiq_MULTIUSE) & landiq_MULTIUSE != "(blank)", .(landiq_MULTIUSE = landiq_MULTIUSE[1L]), keyby = bycols]
  out <- merge(merge(merge(merge(merge(keys, mo, by = bycols, all.x = TRUE), pft, by = bycols, all.x = TRUE), cl, by = bycols, all.x = TRUE), sub, by = bycols, all.x = TRUE), mu, by = bycols, all.x = TRUE)
  out
}

# ---- Build tables by year ----
tables_by_year <- list()
for (yr in names(assign_list)) {
  d <- assign_list[[yr]]
  d[, year := as.integer(year)]
  matched <- d[assigned_by == "matched"]
  fy <- fy_rollup(d)
  fy[is.na(landiq_PFT), landiq_PFT := "(no value)"]
  fy[is.na(landiq_MULTIUSE), landiq_MULTIUSE := "(blank)"]

  # Row-level (matched rows only): MULTIUSE, CLASS, SUBCLASS, season, mslsp_cycle, qc_adoy_vs_cycle
  t_multiuse   <- matched[, .N, by = .(landiq_MULTIUSE)][order(-N)]
  t_class      <- matched[, .N, by = .(landiq_CLASS)][order(-N)]
  t_subclass   <- matched[, .N, by = .(landiq_CLASS, landiq_SUBCLASS)][order(-N)]
  t_season     <- matched[, .N, by = .(season)][order(season)]
  t_cycle      <- matched[, .N, by = .(mslsp_cycle)][order(mslsp_cycle)]
  t_adoy_vs    <- matched[, .N, by = .(qc_adoy_vs_cycle)][order(-N)]
  if ("assigned_woody_tiebreak" %in% names(matched)) matched[, ..wdtb := assigned_woody_tiebreak] else matched[, ..wdtb := NA]
  t_woody_tb   <- matched[, .N, by = "..wdtb"][order(-N)]
  setnames(t_woody_tb, "..wdtb", "assigned_woody_tiebreak")

  # Field-year: match_outcome
  t_outcome    <- fy[!is.na(match_outcome), .N, by = .(match_outcome)][order(-N)]
  t_outcome_pft <- fy[!is.na(match_outcome), .N, by = .(match_outcome, landiq_PFT)][order(match_outcome, -N)]

  tables_by_year[[yr]] <- list(
    year = yr,
    n_row = nrow(d),
    n_matched_rows = nrow(matched),
    n_fy = nrow(fy),
    multiuse = t_multiuse,
    class = t_class,
    subclass = t_subclass,
    season = t_season,
    cycle = t_cycle,
    qc_adoy_vs_cycle = t_adoy_vs,
    assigned_woody_tiebreak = t_woody_tb,
    match_outcome = t_outcome,
    match_outcome_by_pft = t_outcome_pft
  )
}

# Simple table to markdown (no knitr)
tbl_to_md <- function(tab) {
  if (is.null(tab) || nrow(tab) == 0) return("(none)\n")
  nams <- names(tab)
  h <- paste0("| ", paste(nams, collapse = " | "), " |")
  sep <- paste0("|", paste(rep("---", length(nams)), collapse = "|"), "|")
  b <- apply(tab, 1, function(r) paste0("| ", paste(as.character(r), collapse = " | "), " |"))
  paste(c(h, sep, b), collapse = "\n")
}

# ---- Write Markdown report ----
report_date <- format(Sys.Date(), "%Y-%m-%d")
out_md <- file.path(report_dir, paste0("QC_report_", format(Sys.Date(), "%Y%m%d"), ".md"))

sink(out_md)
cat("# LandIQ-MSLSP matching QC report\n\n")
cat("**Generated:** ", report_date, "\n\n")
cat("**Data:** `assigned_year=Y.parquet` in ", matched_dir, "\n\n")
cat("**Years:** ", paste(years, collapse = ", "), "\n\n")
cat("---\n\n")
cat("## 1. Summary of what has been done\n\n")
cat("- **Matching logic** (see `match_landiq_mslsp.R`): LandIQ seasons are matched to MSLSP cycles by (1) ADOY inside [OGI, OGMn]; tie-break by nearest Peak then mslsp_cycle. Season priority: season 2 (main) first when CLASS present; season 1 for MULTIUSE D/M (double/mixed); then 3/4.\n")
cat("- **When ADOY is missing:** Tie-break by season priority and mslsp_cycle (woody and non-woody). Output: `no_adoy_woody_tiebreak` when PFT is woody, else `no_adoy_recorded`. Using ADOY_EMRG as fallback is a possible next step (not in current workflow).\n")
cat("- **Outputs:** One row per parcel-year-season; `assigned_by == \"matched\"` rows have mslsp_cycle and MSLSP dates. Field-year rollup: `match_outcome`, `qc_mslsp_cycles_available`. QC summaries: `qc_summary_year=Y.csv` (by PFT and dimension).\n")
cat("- **Definitions:** ADOY = peak of current season; ADOY_EMRG = peak of next (emerging) season; MULTIUSE D = double (per LandIQ docs). See `LANDIQ_ADOY_SEN_EMRG_notes.md` for details.\n\n")
cat("---\n\n")
cat("## 2. Tables by year\n\n")

for (yr in names(tables_by_year)) {
  tb <- tables_by_year[[yr]]
  cat("### Year ", yr, "\n\n")
  cat("| Metric | Value |\n|--------|-------|\n")
  cat("| Total rows (parcel-year-season) | ", tb$n_row, " |\n")
  cat("| Matched rows | ", tb$n_matched_rows, " |\n")
  cat("| Field-years | ", tb$n_fy, " |\n\n")
  cat("#### Matched rows by MULTIUSE\n\n", tbl_to_md(tb$multiuse), "\n\n")
  cat("#### Matched rows by CLASS\n\n", tbl_to_md(tb$class), "\n\n")
  cat("#### Matched rows by CLASS x SUBCLASS\n\n", tbl_to_md(tb$subclass), "\n\n")
  cat("#### Matched rows by season\n\n", tbl_to_md(tb$season), "\n\n")
  cat("#### Matched rows by assigned MSLSP cycle\n\n", tbl_to_md(tb$cycle), "\n\n")
  cat("#### Matched rows by qc_adoy_vs_cycle\n\n", tbl_to_md(tb$qc_adoy_vs_cycle), "\n\n")
  cat("#### Matched rows: assigned_woody_tiebreak\n\n", tbl_to_md(tb$assigned_woody_tiebreak), "\n\n")
  cat("#### Field-years by match_outcome\n\n", tbl_to_md(tb$match_outcome), "\n\n")
  cat("#### Field-years by match_outcome x PFT\n\n", tbl_to_md(tb$match_outcome_by_pft), "\n\n")
}

cat("---\n\n")
cat("## 3. Ways to improve / things to look into\n\n")
cat("- **Years without ADOY (e.g. 2016):** LandIQ v4.1 has no ADOY for 2016; all such field-years get tie-break (or EMRG where available in later years). No change to logic; document for users.\n")
cat("- **Woody parcels:** Most have no ADOY; we use tie-break only. Rare woody SEN/EMRG are mostly T19 (Bush berries). Consider flagging woody in downstream analyses if needed.\n")
cat("- **Cycle/season mismatches:** `mismatch_2cycles_1season` and `mismatch_1cycle_2seasons` are informational. Review counts by PFT/CLASS; consider targeted checks if high in certain crops.\n")
cat("- **adoy_outside_cycle:** Matched but ADOY outside [OGI, OGMn]. Review by CLASS/SUBCLASS; may indicate double-crop or date convention issues.\n")
cat("- **EMRG as fallback (next step):** Consider using ADOY_EMRG in a cycle window when ADOY is missing (non-woody) to disambiguate cycles; validate with explore_sen_emrg_matching.R and a sample before enabling.\n")
cat("- **QC summary CSV:** Use `extract_qc_summary(assigned_path, out_dir)` to refresh `qc_summary_year=Y.csv` if needed. Report script does not overwrite those.\n")
cat("- **MULTIUSE in assigned:** Added to assigned output in match script so future runs include `landiq_MULTIUSE`; older parquet files may have NA (report then shows \"(blank)\").\n")
sink()

message("Report written: ", out_md)

# Optionally write CSV tables (one per year per dimension) for downstream use
csv_dir <- file.path(report_dir, "qc_report_tables")
dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)
for (yr in names(tables_by_year)) {
  tb <- tables_by_year[[yr]]
  for (nm in c("multiuse", "class", "subclass", "season", "cycle", "qc_adoy_vs_cycle", "assigned_woody_tiebreak", "match_outcome")) {
    t <- tb[[nm]]
    if (!is.null(t) && nrow(t) > 0) {
      fwrite(t, file.path(csv_dir, paste0("year=", yr, "_", nm, ".csv")))
    }
  }
}
message("CSV tables written to: ", csv_dir)
