# Planting events: C/N pools from trait lookup + MSLSP LAI.

build_planting_events <- function(matched, year, out_dir, pool_env, lk) {
  message("[planting] Building events (C/N pools via LAI)")
  if ("planting_date_str" %in% names(matched)) {
    planting_date_str <- as.character(matched$planting_date_str)
  } else {
    planting_date_str <- as.character(matched$mslsp_OGI)
  }
  # Keep rows with a planting date
  keep <- !is.na(planting_date_str) & nzchar(planting_date_str) & planting_date_str != "NA"
  matched <- matched[keep]
  planting_date_str <- planting_date_str[keep]
  message("  Rows with planting date: ", nrow(matched))

  planting_rows <- vector("list", nrow(matched))
  n_skip_no_evi <- 0L
  for (i in seq_len(nrow(matched))) {
    row <- matched[i]
    if (is.na(row$mslsp_EVImax[1]) || is.na(row$mslsp_EVIamp[1])) {
      n_skip_no_evi <- n_skip_no_evi + 1L
      next
    }
    p <- tryCatch(
      pool_env$initialize_planting(
        ID = row$parcel_id,
        DATE = planting_date_str[i],
        PFT = row$landiq_PFT,
        lk = lk,
        class = row$landiq_CLASS,
        subclass = row$landiq_SUBCLASS,
        mslsp_EVImax = row$mslsp_EVImax,
        mslsp_EVIamp = row$mslsp_EVIamp
      ),
      error = function(e) NULL
    )
    if (!is.null(p) && nrow(p) > 0) {
      code <- paste0(trimws(as.character(row$landiq_CLASS)), as.character(row$landiq_SUBCLASS))
      planting_rows[[i]] <- data.table::data.table(
        site_id = row$parcel_id,
        year = row$year,
        season = row$season,
        date = planting_date_str[i],
        code = code,
        PFT = row$landiq_PFT,
        LAI = as.numeric(p$LAI[1]),
        C_LEAF = as.numeric(p$C_LEAF[1]),
        C_STEM = as.numeric(p$C_STEM[1]),
        C_FINEROOT = as.numeric(p$C_FINEROOT[1]),
        C_COARSEROOT = as.numeric(p$C_COARSEROOT[1]),
        N_LEAF = as.numeric(p$N_LEAF[1]),
        N_STEM = as.numeric(p$N_STEM[1]),
        N_FINEROOT = as.numeric(p$N_FINEROOT[1]),
        N_COARSEROOT = as.numeric(p$N_COARSEROOT[1])
      )
    }
    if (i %% 10000L == 0L) {
      message("  ", i, "/", nrow(matched), " done")
    }
  }

  message("  Skipped planting (no MSLSP EVI for LAI): ", n_skip_no_evi)
  planting_dt <- data.table::rbindlist(planting_rows, use.names = TRUE, fill = TRUE)
  data.table::setorder(planting_dt, site_id, year, season)
  planting_dt[, event_type := "planting"]
  data.table::setcolorder(planting_dt, c("event_type", setdiff(names(planting_dt), "event_type")))

  write_event_outputs(
    planting_dt, out_dir, "planting", year,
    json_builder = function(rows, i) {
      list(
        event_type = rows$event_type[i],
        date = rows$date[i],
        year = rows$year[i],
        season = rows$season[i],
        crop = rows$code[i],
        PFT = rows$PFT[i],
        LAI = rows$LAI[i],
        leaf_c_kg_m2 = rows$C_LEAF[i],
        stem_c_kg_m2 = rows$C_STEM[i],
        fineroot_c_kg_m2 = rows$C_FINEROOT[i],
        coarseroot_c_kg_m2 = rows$C_COARSEROOT[i],
        leaf_n_kg_m2 = rows$N_LEAF[i],
        stem_n_kg_m2 = rows$N_STEM[i],
        fineroot_n_kg_m2 = rows$N_FINEROOT[i],
        coarseroot_n_kg_m2 = rows$N_COARSEROOT[i]
      )
    }
  )
  invisible(planting_dt)
}
