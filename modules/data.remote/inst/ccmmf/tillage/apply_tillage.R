#!/usr/bin/env Rscript
# NDTI + matched phenology -> tillage metrics table (tillage_metrics).
# Window: Jan 1 (Y-1) through Dec 31 Y plus HLS_DOWNLOAD_BUFFER_DAYS
# (default 185; forward into Y+1). That is prior-year harvest NDTI plus
# the next green-up. Output year is harvest / OGMn year.
# apply_tillage.R Y writes assigned_year=Y and amends assigned_year=(Y-1)
# so a Y-1 harvest -> Y plant fallow is not dropped when you run TARGET.
# Inventory pair: apply PRIOR then TARGET (TARGET refreshes PRIOR).
# $TARGET_YEAR harvest rows stay partial until the next update.
#
# Math: events/R/tillage_metrics.R, modules/data.land/R/ndti_to_sipnet_tillage.R
# ENV: MATCHED_DIR, HLS_DOWNLOAD_BUFFER_DAYS, TILLAGE_PARCEL_CHUNK
# Usage: Rscript apply_tillage.R <year>

# Load PEcAn NDTI-drop -> tillage_eff_0to1 mapper (fractional drop in [0, 1]).
load_ndti_to_sipnet_tillage <- function() {
  if (exists("ndti_to_sipnet_tillage", mode = "function", inherits = TRUE)) {
    return(invisible(TRUE))
  }
  if (requireNamespace("PEcAn.data.land", quietly = TRUE)) {
    ndti_to_sipnet_tillage <<- PEcAn.data.land::ndti_to_sipnet_tillage
    return(invisible(TRUE))
  }
  code <- trimws(Sys.getenv("CCMMF_CODE", ""))
  if (!nzchar(code)) {
    stop("Need PEcAn.data.land::ndti_to_sipnet_tillage or CCMMF_CODE.")
  }
  src <- file.path(
    dirname(dirname(dirname(normalizePath(code, mustWork = FALSE)))),
    "data.land", "R", "ndti_to_sipnet_tillage.R"
  )
  if (!file.exists(src)) {
    stop("Missing ndti_to_sipnet_tillage.R: ", src)
  }
  if (!requireNamespace("PEcAn.logger", quietly = TRUE)) {
    stop("PEcAn.logger is required to source ndti_to_sipnet_tillage.R")
  }
  source(src, local = FALSE)
  invisible(TRUE)
}

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

load_assigned_years_for_tillage <- function(years, matched_dir) {
  gapfill_dir <- Sys.getenv(
    "GAPFILL_DATES_DIR",
    file.path(matched_dir, "gapfill_dates")
  )
  parts <- list()
  for (y in years) {
    y <- as.integer(y)
    gapfill_file <- file.path(gapfill_dir, sprintf("assigned_year=%d_gapfilled.parquet", y))
    assigned_file <- file.path(matched_dir, sprintf("assigned_year=%d.parquet", y))
    src <- NA_character_
    path_use <- NA_character_
    if (file.exists(gapfill_file) && file.exists(assigned_file)) {
      if (file.info(gapfill_file)$mtime >= file.info(assigned_file)$mtime) {
        src <- "gapfilled"
        path_use <- gapfill_file
      } else {
        src <- "mslsp_assigned"
        path_use <- assigned_file
        message(
          "[tillage] year=", y, " gapfilled older than assigned; using mslsp assigned: ",
          assigned_file
        )
      }
    } else if (file.exists(gapfill_file)) {
      src <- "gapfilled"
      path_use <- gapfill_file
    } else if (file.exists(assigned_file)) {
      src <- "mslsp_assigned"
      path_use <- assigned_file
    } else {
      message("[tillage] skip missing assigned/gapfill for year=", y)
      next
    }
    message("[tillage] year=", y, " phenology from ", src, " path=", path_use)
    parts[[length(parts) + 1L]] <- data.table::as.data.table(arrow::read_parquet(path_use))
  }
  if (length(parts) == 0L) {
    return(data.table::data.table())
  }

  mslsp_all <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  mslsp_all[, parcel_id := as.character(parcel_id)]

  has_gapfill_src <- "gapfill_date_source" %in% names(mslsp_all) ||
    all(c("gapfill_planting_source", "gapfill_harvest_source") %in% names(mslsp_all))

  if (has_gapfill_src) {
    mslsp_all <- mslsp_all[assigned_by %in% c("matched", "no_mslsp", "no_match")]
  } else {
    mslsp_all <- mslsp_all[assigned_by == "matched"]
  }

  if ("landiq_PFT" %in% names(mslsp_all)) {
    is_other <- tolower(trimws(as.character(mslsp_all$landiq_PFT))) == "other"
    if (any(is_other, na.rm = TRUE)) {
      message("[tillage] skipping ", sum(is_other, na.rm = TRUE), " other-PFT row(s)")
      mslsp_all <- mslsp_all[!is_other]
    }
  }

  if (!"gapfill_date_source" %in% names(mslsp_all)) {
    mslsp_all[, gapfill_date_source := NA_character_]
  }
  mslsp_all[
    is.na(gapfill_date_source) | !nzchar(as.character(gapfill_date_source)),
    gapfill_date_source := data.table::fifelse(
      assigned_by == "matched",
      "mslsp",
      "none"
    )
  ]
  mslsp_all[, gapfill_date_source := as.character(gapfill_date_source)]
  mslsp_all[, assigned_by := as.character(assigned_by)]

  mslsp_all <- mslsp_all[!is.na(mslsp_OGI) | !is.na(mslsp_OGMn)]
  mslsp_all[, OGI_date := as.Date(mslsp_OGI)]
  mslsp_all[, OGMn_date := as.Date(mslsp_OGMn)]
  mslsp_all <- mslsp_all[!is.na(OGI_date) | !is.na(OGMn_date)]
  mslsp_all
}

tillage_hls_window <- function(year) {
  buffer_days <- suppressWarnings(
    as.integer(Sys.getenv("HLS_DOWNLOAD_BUFFER_DAYS", "185"))
  )
  if (is.na(buffer_days) || buffer_days < 0L) {
    buffer_days <- 185L
  }
  year <- as.integer(year)
  start <- as.Date(sprintf("%d-01-01", year - 1L))
  end <- as.Date(sprintf("%d-12-31", year)) + buffer_days
  list(
    buffer_days = buffer_days,
    start = start,
    end = end,
    years = seq.int(
      as.integer(format(start, "%Y")),
      as.integer(format(end, "%Y"))
    )
  )
}

build_tillage_metrics_table <- function(year, matched_dir, ndti_root,
                                        tillage_metrics_script, metrics_dir) {
  suppressPackageStartupMessages(library(dplyr))
  load_ndti_to_sipnet_tillage()
  if (!file.exists(tillage_metrics_script)) {
    stop("Missing tillage_metrics.R: ", tillage_metrics_script)
  }
  source(tillage_metrics_script)

  year_arg <- as.integer(year)
  win <- tillage_hls_window(year_arg)
  load_years <- win$years
  chunk_n <- suppressWarnings(as.integer(Sys.getenv("TILLAGE_PARCEL_CHUNK", "3000")))
  if (is.na(chunk_n) || chunk_n < 1L) {
    chunk_n <- 3000L
  }

  message(
    "[tillage] job year=", year_arg,
    " | window ", format(win$start), " to ", format(win$end),
    " (buffer=", win$buffer_days, " d) | NDTI/phenology years ",
    min(load_years), ":", max(load_years),
    " | chunk ", chunk_n
  )

  list_ndti_parquet <- function(yrs) {
    fl <- character(0)
    for (y in yrs) {
      ydir <- file.path(ndti_root, sprintf("year=%d", y))
      if (!dir.exists(ydir)) {
        next
      }
      fl <- c(
        fl,
        Sys.glob(file.path(ydir, sprintf("ndti_year=%d_month=*.parquet", y))),
        Sys.glob(file.path(ydir, "*.parquet"))
      )
    }
        unique(fl[file.exists(fl) & file.info(fl)$size > 0])
  }
  ndti_files <- list_ndti_parquet(load_years)
  if (length(ndti_files) == 0L) {
    stop("[tillage] No NDTI parquet under ", ndti_root)
  }
  message("[tillage] NDTI files found: ", length(ndti_files))

  mslsp_all <- load_assigned_years_for_tillage(load_years, matched_dir)
  if (nrow(mslsp_all) == 0L) {
    stop("[tillage] No assigned/gapfill parquet for load years")
  }

  phenology_full <- mslsp_all[, .(
    parcel_id,
    year,
    OGI_date,
    OGMn_date,
    assigned_by,
    gapfill_date_source
  )]
  pft_y <- mslsp_all[, .(PFT = landiq_PFT[1L]), by = .(parcel_id, year)]

  message(
    "[tillage] phenology rows ", nrow(phenology_full),
    " | parcels ", data.table::uniqueN(phenology_full$parcel_id)
  )

  read_ndti_for_parcels <- function(parcel_ids, yrs, root) {
    pid_unique <- unique(as.character(parcel_ids))
    parts <- list()
    for (y in yrs) {
      ydir <- file.path(root, sprintf("year=%d", y))
      if (!dir.exists(ydir)) {
        next
      }
      fl <- c(
        Sys.glob(file.path(ydir, sprintf("ndti_year=%d_month=*.parquet", y))),
        Sys.glob(file.path(ydir, "*.parquet"))
      )
      fl <- unique(fl[file.exists(fl) & file.info(fl)$size > 0])
      if (length(fl) == 0L) {
        next
      }
      ds <- tryCatch(arrow::open_dataset(fl), error = function(e) NULL)
      if (is.null(ds)) {
        next
      }
      sub <- tryCatch(
        ds |>
          dplyr::filter(parcel_id %in% pid_unique) |>
          dplyr::collect(),
        error = function(e) NULL
      )
      if (!is.null(sub) && nrow(sub) > 0L) {
        parts[[length(parts) + 1L]] <- data.table::as.data.table(sub)
      }
    }
    if (length(parts) == 0L) {
      return(data.table::data.table())
    }
    data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
  }

  all_pids <- unique(phenology_full$parcel_id)
  n_chunk <- ceiling(length(all_pids) / chunk_n)
  results <- vector("list", n_chunk)

  for (ic in seq_len(n_chunk)) {
    i0 <- (ic - 1L) * chunk_n + 1L
    i1 <- min(ic * chunk_n, length(all_pids))
    pchunk <- all_pids[i0:i1]
    message("[tillage] chunk ", ic, "/", n_chunk, " parcels ", i0, ":", i1)

    pheno_chunk <- phenology_full[parcel_id %in% pchunk]
    if (nrow(pheno_chunk) == 0L) {
      next
    }

    ndti_chunk <- read_ndti_for_parcels(pchunk, load_years, ndti_root)
    if (nrow(ndti_chunk) == 0L) {
      message("  no NDTI rows")
      next
    }
    ndti_chunk[, date := as.Date(date)]
    ndti_chunk <- ndti_chunk[date >= win$start & date <= win$end]
    if (nrow(ndti_chunk) == 0L) {
      message("  no NDTI rows in window")
      next
    }
    ndti_chunk <- merge(ndti_chunk, pft_y, by = c("parcel_id", "year"), all.x = TRUE)
    ndti_chunk <- ndti_chunk[!is.na(PFT) & nzchar(as.character(PFT))]

    common <- intersect(unique(ndti_chunk$parcel_id), unique(pheno_chunk$parcel_id))
    if (length(common) == 0L) {
      message("  no ndti/phenology overlap")
      next
    }
    ndti_chunk <- ndti_chunk[parcel_id %in% common]
    pheno_chunk <- pheno_chunk[parcel_id %in% common]

    res <- tryCatch(
      tillage_metrics(ndti_table = ndti_chunk, phenology_table = pheno_chunk),
      error = function(e) {
        warning("[tillage] tillage_metrics failed chunk ", ic, ": ", conditionMessage(e))
        NULL
      }
    )
    if (!is.null(res) && nrow(res) > 0L) {
      results[[ic]] <- data.table::as.data.table(res)
    }
  }

  all_res <- data.table::rbindlist(results[!vapply(results, is.null, NA)], use.names = TRUE, fill = TRUE)
  if (nrow(all_res) == 0L) {
    stop("[tillage] No results (check NDTI overlap and errors above)")
  }

  all_res[, parcel_id := as.character(parcel_id)]
  all_res[, year := as.integer(year)]

  prepare_tillage_out <- function(yr_dt) {
    n_pre <- nrow(yr_dt)
    ord_cols <- intersect(c("parcel_id", "OGMn_date", "min_date", "max_date"), names(yr_dt))
    if (length(ord_cols) > 0L) {
      data.table::setorderv(yr_dt, ord_cols)
    }
    yr_dt <- unique(yr_dt, by = c("parcel_id", "OGMn_date"))
    if (nrow(yr_dt) < n_pre) {
      message(
        "[tillage] deduped ", n_pre - nrow(yr_dt),
        " duplicate row(s) (parcel_id + OGMn_date); kept first after sort"
      )
    }
    out_dt <- data.table::copy(yr_dt)
    out_dt[, site_id := parcel_id]
    out_dt[, event_type := "tillage"]
    # tillage_metrics() stores ndti_pct_change as percent (0-100); mapper wants [0, 1].
    delta <- as.numeric(out_dt$ndti_pct_change) / 100
    out_dt[, tillage_eff_0to1 := ndti_to_sipnet_tillage(delta)]
    date_cols <- names(out_dt)[vapply(out_dt, function(z) inherits(z, "Date"), NA)]
    for (cn in date_cols) {
      out_dt[, (cn) := as.character(get(cn))]
    }
    data.table::setcolorder(out_dt, c("event_type", setdiff(names(out_dt), "event_type")))
    out_dt
  }

  dir.create(metrics_dir, recursive = TRUE, showWarnings = FALSE)

  # Harvest year Y-1 (closed by this year's OGI) and Y (partial if Y+1
  # phenology is missing). Do not drop Y-1: that is the pair update.
  years_out <- sort(unique(as.integer(all_res$year)))
  years_out <- years_out[years_out %in% c(year_arg - 1L, year_arg)]
  if (length(years_out) == 0L) {
    message("[tillage] year ", year_arg, ": no harvest-year rows; writing empty table")
    out <- tillage_table_path(metrics_dir, year_arg)
    empty <- data.table::data.table(
      event_type = character(),
      parcel_id = character(),
      site_id = character(),
      year = integer(),
      tillage_eff_0to1 = numeric()
    )
    arrow::write_parquet(empty, out)
    message("[tillage] wrote ", out)
    return(invisible(all_res))
  }

  for (y in years_out) {
    yr_dt <- all_res[year == y]
    out <- tillage_table_path(metrics_dir, y)
    if (nrow(yr_dt) == 0L) {
      next
    }
    out_dt <- prepare_tillage_out(yr_dt)
    arrow::write_parquet(out_dt, out)
    extra <- if (identical(as.integer(y), year_arg - 1L)) " (amended prior harvest year)" else ""
    message("[tillage] wrote ", out, " (", nrow(out_dt), " rows)", extra)
  }
  invisible(all_res)
}

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  stop("Usage: Rscript apply_tillage.R <year>")
}
year_arg <- as.integer(args[1L])
if (is.na(year_arg)) {
  stop("Year must be an integer, got: ", args[1L])
}

events_root <- trimws(Sys.getenv("EVENTS_ROOT", ""))
code <- trimws(Sys.getenv("CCMMF_CODE", ""))
if (!nzchar(events_root)) {
  if (!nzchar(code)) {
    stop("Set EVENTS_ROOT or CCMMF_CODE (source documentation/setup_env.sh).")
  }
  events_root <- file.path(code, "events")
}

source(file.path(events_root, "R", "paths.R"))

paths <- events_paths()
build_tillage_metrics_table(
  year_arg,
  paths$matched_dir,
  paths$ndti_root,
  paths$tillage_metrics_script,
  paths$tillage_metrics_dir
)
