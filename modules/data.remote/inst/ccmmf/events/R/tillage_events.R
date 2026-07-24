# Tillage events: NDTI + multi-year phenology via tillage_metrics().
# tillage_metrics() is loaded by R/bootstrap.R (same pattern as planting/harvest).

build_tillage_events <- function(year, out_dir, matched_dir, ndti_root) {
  suppressPackageStartupMessages(library(dplyr))
  if (!exists("tillage_metrics", mode = "function")) {
    stop("tillage_metrics() not loaded; source R/tillage_metrics.R via bootstrap")
  }

  year_arg <- as.integer(year)
  buf <- suppressWarnings(as.integer(Sys.getenv("TILLAGE_BUFFER_YEARS", "1")))
  if (is.na(buf) || buf < 0L) {
    buf <- 1L
  }
  chunk_n <- suppressWarnings(as.integer(Sys.getenv("TILLAGE_PARCEL_CHUNK", "3000")))
  if (is.na(chunk_n) || chunk_n < 1L) {
    chunk_n <- 3000L
  }

  year_first <- year_arg
  year_last <- year_arg
  load_years <- seq(year_first - buf, year_last + buf)

  message(
    "[tillage] output year ", year_arg, " | load years ", min(load_years), ":", max(load_years),
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
    unique(fl[file.exists(fl)])
  }
  ndti_files <- list_ndti_parquet(load_years)
  if (length(ndti_files) == 0L) {
    stop("[tillage] No NDTI parquet under ", ndti_root)
  }
  message("[tillage] NDTI files found: ", length(ndti_files))

  mslsp_parts <- list()
  for (y in load_years) {
    f <- file.path(matched_dir, sprintf("assigned_year=%d.parquet", y))
    if (!file.exists(f)) {
      message("[tillage] skip missing ", f)
      next
    }
    mslsp_parts[[length(mslsp_parts) + 1L]] <- data.table::as.data.table(arrow::read_parquet(f))
  }
  if (length(mslsp_parts) == 0L) {
    stop("[tillage] No assigned parquet for load years")
  }

  mslsp_all <- data.table::rbindlist(mslsp_parts, use.names = TRUE, fill = TRUE)
  mslsp_all[, parcel_id := as.character(parcel_id)]
  mslsp_all <- mslsp_all[assigned_by == "matched"]
  mslsp_all <- mslsp_all[!is.na(mslsp_OGI) & !is.na(mslsp_OGMn)]
  mslsp_all[, OGI_date := as.Date(mslsp_OGI)]
  mslsp_all[, OGMn_date := as.Date(mslsp_OGMn)]
  mslsp_all <- mslsp_all[!is.na(OGI_date) & !is.na(OGMn_date)]

  phenology_full <- mslsp_all[, .(parcel_id, year, OGI_date, OGMn_date)]
  pft_y <- mslsp_all[, .(PFT = landiq_PFT[1L]), by = .(parcel_id, year)]

  message("[tillage] phenology rows ", nrow(phenology_full), " | parcels ", data.table::uniqueN(phenology_full$parcel_id))

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

  for (y in year_first:year_last) {
    yr_dt <- all_res[year == y]
    paths <- event_output_paths(out_dir, "tillage", y)

    if (nrow(yr_dt) == 0L) {
      message("[tillage] year ", y, ": no rows; writing empty outputs")
      arrow::write_parquet(
        data.table::data.table(event_type = character(), parcel_id = character(), year = integer()),
        paths$parquet
      )
      writeLines("{}", paths$json)
      next
    }

    n_pre <- nrow(yr_dt)
    ord_cols <- intersect(c("parcel_id", "OGMn_date", "min_date", "max_date"), names(yr_dt))
    if (length(ord_cols) > 0L) {
      data.table::setorderv(yr_dt, ord_cols)
    }
    yr_dt <- unique(yr_dt, by = c("parcel_id", "OGMn_date"))
    if (nrow(yr_dt) < n_pre) {
      message(
        "[tillage] year ", y, ": deduped ", n_pre - nrow(yr_dt),
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

    write_event_outputs(
      out_dt, out_dir, "tillage", y,
      json_builder = function(rows, i) {
        list(
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
      }
    )
  }

  invisible(all_res)
}
