# Planting events: C/N pools from trait lookup + MSLSP LAI.
# When MSLSP EVI is missing (gap-filled dates / empty cycles), use a CLASS->PFT->
# global median-EVI LAI fallback from parcels that do have EVI in the same year.
# Young woody (CLASS=YP or SPECOND=Y) is phenology-only -- no planting events.

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

  # Idle/fallow (PFT other) is not a crop planting event
  is_other <- tolower(trimws(as.character(matched$landiq_PFT))) == "other"
  n_skip_other <- sum(is_other, na.rm = TRUE)
  if (n_skip_other > 0L) {
    matched <- matched[!is_other]
    planting_date_str <- planting_date_str[!is_other]
    message("  Skipped planting (PFT other / idle-fallow): ", n_skip_other)
  }

  # Young woody: phenology only (same rule as harvest)
  specond <- if ("landiq_SPECOND" %in% names(matched)) {
    as.character(matched$landiq_SPECOND)
  } else {
    rep(NA_character_, nrow(matched))
  }
  is_yp <- tolower(trimws(as.character(matched$landiq_PFT))) == "woody" &
    (
      toupper(trimws(as.character(matched$landiq_CLASS))) == "YP" |
        toupper(trimws(specond)) == "Y"
    )
  n_skip_yp <- sum(is_yp, na.rm = TRUE)
  if (n_skip_yp > 0L) {
    matched <- matched[!is_yp]
    planting_date_str <- planting_date_str[!is_yp]
    message("  Skipped planting (young woody YP / SPECOND=Y; phenology-only): ", n_skip_yp)
  }

  lai_fb <- planting_lai_fallbacks(matched, pool_env)
  message(
    "  LAI fallbacks ready: class-pft=", nrow(lai_fb$class_pft),
    " pft=", nrow(lai_fb$pft),
    " global=", if (is.finite(lai_fb$global)) round(lai_fb$global, 3) else "NA"
  )

  planting_rows <- vector("list", nrow(matched))
  n_lai_from_evi <- 0L
  n_lai_fallback <- 0L
  n_skip_no_lai <- 0L
  for (i in seq_len(nrow(matched))) {
    row <- matched[i]
    mx <- suppressWarnings(as.numeric(row$mslsp_EVImax[1]))
    ma <- suppressWarnings(as.numeric(row$mslsp_EVIamp[1]))
    use_evi <- is.finite(mx) && is.finite(ma)

    if (use_evi) {
      n_lai_from_evi <- n_lai_from_evi + 1L
      lai_source <- "mslsp_evi"
      p <- tryCatch(
        pool_env$initialize_planting(
          ID = row$parcel_id,
          DATE = planting_date_str[i],
          PFT = row$landiq_PFT,
          lk = lk,
          class = row$landiq_CLASS,
          subclass = row$landiq_SUBCLASS,
          mslsp_EVImax = mx,
          mslsp_EVIamp = ma,
          diagnostics = TRUE
        ),
        error = function(e) NULL
      )
    } else {
      lai_arg <- lookup_planting_lai_fallback(
        class = row$landiq_CLASS[1],
        pft = row$landiq_PFT[1],
        fb = lai_fb
      )
      if (!is.finite(lai_arg)) {
        n_skip_no_lai <- n_skip_no_lai + 1L
        next
      }
      n_lai_fallback <- n_lai_fallback + 1L
      lai_source <- "lai_fallback"
      p <- tryCatch(
        pool_env$initialize_planting(
          ID = row$parcel_id,
          DATE = planting_date_str[i],
          PFT = row$landiq_PFT,
          lk = lk,
          class = row$landiq_CLASS,
          subclass = row$landiq_SUBCLASS,
          LAI = lai_arg,
          diagnostics = TRUE
        ),
        error = function(e) NULL
      )
    }

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
        lai_source = lai_source,
        C_LEAF = as.numeric(p$C_LEAF[1]),
        C_STEM = as.numeric(p$C_STEM[1]),
        C_FINEROOT = as.numeric(p$C_FINEROOT[1]),
        C_COARSEROOT = as.numeric(p$C_COARSEROOT[1]),
        N_LEAF = as.numeric(p$N_LEAF[1]),
        N_STEM = as.numeric(p$N_STEM[1]),
        N_FINEROOT = as.numeric(p$N_FINEROOT[1]),
        N_COARSEROOT = as.numeric(p$N_COARSEROOT[1]),
        sla_src = as.character(p$sla_src[1]),
        src_9 = as.character(p$src_9[1]),
        src_470 = as.character(p$src_470[1]),
        src_2005 = as.character(p$src_2005[1]),
        src_1534 = as.character(p$src_1534[1]),
        src_1019 = as.character(p$src_1019[1]),
        root_split_src = as.character(p$root_split_src[1]),
        used_default_split = isTRUE(p$used_default_split[1]),
        used_lit_any = isTRUE(p$used_lit_any[1]),
        used_pft_any = isTRUE(p$used_pft_any[1]),
        used_class_any = isTRUE(p$used_class_any[1])
      )
    }
    if (i %% 10000L == 0L) {
      message("  ", i, "/", nrow(matched), " done")
    }
  }

  message("  Planting LAI from MSLSP EVI: ", n_lai_from_evi)
  message("  Planting LAI from class/PFT fallback (gap-filled dates): ", n_lai_fallback)
  message("  Skipped planting (no LAI fallback available): ", n_skip_no_lai)
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
        coarseroot_n_kg_m2 = rows$N_COARSEROOT[i],
        # Provenance (optional for SIPNET; kept for audit)
        lai_source = rows$lai_source[i],
        sla_src = rows$sla_src[i],
        src_9 = rows$src_9[i],
        src_470 = rows$src_470[i],
        src_2005 = rows$src_2005[i],
        src_1534 = rows$src_1534[i],
        root_split_src = rows$root_split_src[i],
        used_default_split = rows$used_default_split[i]
      )
    }
  )
  invisible(planting_dt)
}

# Median EVI -> LAI by CLASS+PFT and by PFT; global median LAI.
planting_lai_fallbacks <- function(matched, pool_env) {
  empty <- list(
    class_pft = data.table::data.table(
      landiq_CLASS = character(), pft = character(), lai_fb = numeric()
    ),
    pft = data.table::data.table(pft = character(), lai_fb = numeric()),
    global = NA_real_
  )
  if (!nrow(matched)) {
    return(empty)
  }
  has_evi <- is.finite(suppressWarnings(as.numeric(matched$mslsp_EVImax))) &
    is.finite(suppressWarnings(as.numeric(matched$mslsp_EVIamp)))
  if (!any(has_evi)) {
    return(empty)
  }

  evi <- matched[has_evi]
  evi[, `:=`(
    landiq_CLASS = trimws(as.character(landiq_CLASS)),
    pft = tolower(trimws(as.character(landiq_PFT))),
    mx = as.numeric(mslsp_EVImax),
    ma = as.numeric(mslsp_EVIamp)
  )]
  grp <- evi[
    nzchar(landiq_CLASS) & nzchar(pft),
    .(mx = stats::median(mx, na.rm = TRUE), ma = stats::median(ma, na.rm = TRUE)),
    by = .(landiq_CLASS, pft)
  ]
  if (!nrow(grp)) {
    return(empty)
  }

  lai_fun <- pool_env$compute_lai_from_mslsp
  if (!is.function(lai_fun)) {
    lai_fun <- get("compute_lai_from_mslsp", inherits = TRUE)
  }
  lai_one <- function(mx, ma, pft, class) {
    tryCatch(
      as.numeric(lai_fun(
        mslsp_EVImax = mx,
        mslsp_EVIamp = ma,
        pft = pft,
        class = class
      )[1]),
      error = function(e) NA_real_
    )
  }
  grp[, lai_fb := mapply(lai_one, mx, ma, pft, landiq_CLASS)]
  grp <- grp[is.finite(lai_fb)]
  if (!nrow(grp)) {
    return(empty)
  }

  pft_fb <- grp[, .(lai_fb = stats::median(lai_fb, na.rm = TRUE)), by = pft]
  list(
    class_pft = grp[, .(landiq_CLASS, pft, lai_fb)],
    pft = pft_fb,
    global = stats::median(grp$lai_fb, na.rm = TRUE)
  )
}

lookup_planting_lai_fallback <- function(class, pft, fb) {
  cls <- trimws(as.character(class)[1])
  pft_l <- tolower(trimws(as.character(pft)[1]))
  if (nzchar(cls) && nzchar(pft_l) && nrow(fb$class_pft)) {
    hit <- fb$class_pft[landiq_CLASS == cls & pft == pft_l, lai_fb]
    if (length(hit) && is.finite(hit[1])) {
      return(hit[1])
    }
  }
  if (nzchar(pft_l) && nrow(fb$pft)) {
    hit <- fb$pft[pft == pft_l, lai_fb]
    if (length(hit) && is.finite(hit[1])) {
      return(hit[1])
    }
  }
  fb$global
}
