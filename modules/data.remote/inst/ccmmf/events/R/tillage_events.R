# Tillage events: NDTI + multi-year phenology via tillage_metrics().
# Prefer gapfilled product (same as planting/harvest) so OGI/OGMn exist for
# no_match / no_mslsp crop rows; fall back to mslsp assigned when absent.

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

  mslsp_all <- mslsp_all[!is.na(mslsp_OGI) & !is.na(mslsp_OGMn)]
  mslsp_all[, OGI_date := as.Date(mslsp_OGI)]
  mslsp_all[, OGMn_date := as.Date(mslsp_OGMn)]
  mslsp_all <- mslsp_all[!is.na(OGI_date) & !is.na(OGMn_date)]
  mslsp_all
}

build_tillage_events <- function(year, out_dir, matched_dir, ndti_root, tillage_metrics_script) {
  suppressPackageStartupMessages(library(dplyr))
  if (!file.exists(tillage_metrics_script)) {
    stop("Missing tillage_metrics.R: ", tillage_metrics_script)
  }
  source(tillage_metrics_script)

  year_arg <- as.integer(year)
  buf <- suppressWarnings(as.integer(Sys.getenv("TILLAGE_BUFFER_YEARS", "1")))
  if (is.na(buf) || buf < 0L) {
    buf <- 1L
  }
  chunk_n <- suppressWarnings(as.integer(Sys.getenv("TILLAGE_PARCEL_CHUNK", "3000")))
  if (is.na(chunk_n) || chunk_n < 1L) {
    chunk_n <- 3000L
  }

  # Lookback only: load (Y - buffer):Y. Cross-year fallows that end in Y
  # (OGMn in Y-1 -> OGI in Y) are written when this job runs; fallows that need
  # Y+1 are finalized by the next year's job (which rewrites year Y).
  year_first <- year_arg - buf
  year_last <- year_arg
  load_years <- seq(year_first, year_last)

  message(
    "[tillage] job year=", year_arg,
    " | load/write years ", min(load_years), ":", max(load_years),
    " (lookback=", buf, ") | chunk ", chunk_n
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
    unique(fl[file.exists(fl)])
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
    " | parcels ", data.table::uniqueN(phenology_full$parcel_id),
    " | gapfill_date_source: ",
    paste(names(table(phenology_full$gapfill_date_source, useNA = "ifany")),
          as.integer(table(phenology_full$gapfill_date_source, useNA = "ifany")),
          collapse = ", ")
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
      fl <- unique(fl[file.exists(fl)])
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

  tillage_json_builder <- function(rows, i) {
    out <- list(
      event_type = rows$event_type[i],
      year = rows$year[i],
      PFT = rows$PFT[i],
      OGMn_date = rows$OGMn_date[i],
      max_date = rows$max_date[i],
      max_ndti = rows$max_ndti[i],
      min_date = rows$min_date[i],
      min_ndti = rows$min_ndti[i],
      min_n_valid = rows$min_n_valid[i],
      min_sd = rows$min_sd[i],
      ndti_pct_change = rows$ndti_pct_change[i],
      min_val_date_before = rows$min_val_date_before[i],
      min_val_n_before = rows$min_val_n_before[i],
      min_val_date_after = rows$min_val_date_after[i],
      min_val_n_after = rows$min_val_n_after[i]
    )
    for (cn in c(
      "ogmn_assigned_by", "ogmn_source",
      "ogi_assigned_by", "ogi_source"
    )) {
      if (cn %in% names(rows)) {
        out[[cn]] <- rows[[cn]][i]
      }
    }
    out
  }

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
    date_cols <- names(out_dt)[vapply(out_dt, function(z) inherits(z, "Date"), NA)]
    for (cn in date_cols) {
      out_dt[, (cn) := as.character(get(cn))]
    }
    data.table::setcolorder(out_dt, c("event_type", setdiff(names(out_dt), "event_type")))
    out_dt
  }

  for (y in year_first:year_last) {
    yr_dt <- all_res[year == y]
    is_job_year <- identical(as.integer(y), as.integer(year_arg))

    if (nrow(yr_dt) == 0L) {
      if (!is_job_year) {
        message("[tillage] year ", y, ": no lookback rows; skip amend")
        next
      }
      message("[tillage] year ", y, ": no rows; writing empty outputs")
      paths <- event_output_paths(out_dir, "tillage", y)
      arrow::write_parquet(
        data.table::data.table(event_type = character(), parcel_id = character(), year = integer()),
        paths$parquet
      )
      writeLines("{}", paths$json)
      next
    }

    out_dt <- prepare_tillage_out(yr_dt)

    if (is_job_year) {
      # Canonical product for this year (safe under parallel year jobs).
      write_event_outputs(
        out_dt, out_dir, "tillage", y,
        json_builder = tillage_json_builder
      )
    } else {
      # Cross-year fallows (OGMn in y, OGI in year_arg). Side file so parallel
      # jobs never race on tillage_statewide_y.parquet; merge afterward.
      amend <- tillage_lookback_amend_path(out_dir, y, year_arg)
      arrow::write_parquet(out_dt, amend)
      message("[tillage] wrote lookback amend ", amend, " (", nrow(out_dt), " rows)")
    }
  }

  invisible(all_res)
}

#' Merge job-year products with lookback amends from the next year (parallel-safe).
merge_tillage_lookback <- function(out_dir, years) {
  years <- sort(unique(as.integer(years)))
  for (y in years) {
    paths <- event_output_paths(out_dir, "tillage", y)
    amend <- tillage_lookback_amend_path(out_dir, y, y + 1L)

    has_main <- file.exists(paths$parquet)
    has_amend <- file.exists(amend)
    if (!has_main && !has_amend) {
      message("[tillage-merge] year=", y, ": nothing to merge; skip")
      next
    }
    if (has_main && !has_amend) {
      message("[tillage-merge] year=", y, ": canonical only; leave as-is")
      next
    }

    parts <- list()
    if (has_main) {
      main_dt <- data.table::as.data.table(arrow::read_parquet(paths$parquet))
      main_dt[, .merge_rank := 0L]
      parts[[length(parts) + 1L]] <- main_dt
    }
    message("[tillage-merge] year=", y, " + amend from ", y + 1L)
    amend_dt <- data.table::as.data.table(arrow::read_parquet(amend))
    amend_dt[, .merge_rank := 1L]
    parts[[length(parts) + 1L]] <- amend_dt

    yr_dt <- data.table::rbindlist(parts, use.names = TRUE, fill = TRUE)
    n_pre <- nrow(yr_dt)
    data.table::setorderv(yr_dt, c("parcel_id", "OGMn_date", ".merge_rank"), order = c(1L, 1L, -1L))
    yr_dt <- unique(yr_dt, by = c("parcel_id", "OGMn_date"))
    yr_dt[, .merge_rank := NULL]
    if (nrow(yr_dt) < n_pre) {
      message("[tillage-merge] year=", y, ": deduped ", n_pre - nrow(yr_dt), " row(s)")
    }
    if (!("site_id" %in% names(yr_dt))) {
      yr_dt[, site_id := parcel_id]
    }
    if (!("event_type" %in% names(yr_dt))) {
      yr_dt[, event_type := "tillage"]
    }

    write_event_outputs(
      yr_dt, out_dir, "tillage", y,
      json_builder = function(rows, i) {
        out <- list(
          event_type = rows$event_type[i],
          year = rows$year[i],
          PFT = rows$PFT[i],
          OGMn_date = rows$OGMn_date[i],
          max_date = rows$max_date[i],
          max_ndti = rows$max_ndti[i],
          min_date = rows$min_date[i],
          min_ndti = rows$min_ndti[i],
          min_n_valid = rows$min_n_valid[i],
          min_sd = rows$min_sd[i],
          ndti_pct_change = rows$ndti_pct_change[i],
          min_val_date_before = rows$min_val_date_before[i],
          min_val_n_before = rows$min_val_n_before[i],
          min_val_date_after = rows$min_val_date_after[i],
          min_val_n_after = rows$min_val_n_after[i]
        )
        for (cn in c(
          "ogmn_assigned_by", "ogmn_source",
          "ogi_assigned_by", "ogi_source"
        )) {
          if (cn %in% names(rows)) {
            out[[cn]] <- rows[[cn]][i]
          }
        }
        out
      }
    )
  }
  invisible(NULL)
}
