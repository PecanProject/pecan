# Shared EVI -> LAI and LAI -> C/N tables for planting.
# CLI: apply_planting.R
# make_events_statewide planting reads assigned_year=Y_planting.parquet
# (does not recompute). Hay and woody are not planted here.

planting_keep_dated_crop_rows <- function(matched) {
  if (!"planting_date_str" %in% names(matched)) {
    matched[, planting_date_str := as.character(mslsp_OGI)]
  }
  d <- as.character(matched$planting_date_str)
  keep <- !is.na(d) & nzchar(d) & d != "NA"
  matched <- matched[keep]
  pft_l <- tolower(trimws(as.character(matched$landiq_PFT)))
  # Annual planting only (row, rice). Hay and woody are not planted each year;
  # they get phenology + harvest instead. YP OGI stays on the overlay.
  matched <- matched[!pft_l %in% c("other", "woody", "hay")]
  matched
}

# Mean EVImax -> LAI by CLASS+PFT and by PFT; global mean LAI.
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
  has_evi <- is.finite(suppressWarnings(as.numeric(matched$mslsp_EVImax)))
  if (!any(has_evi)) {
    return(empty)
  }

  evi <- matched[has_evi]
  evi[, `:=`(
    landiq_CLASS = trimws(as.character(landiq_CLASS)),
    pft = tolower(trimws(as.character(landiq_PFT))),
    mx = as.numeric(mslsp_EVImax)
  )]
  grp <- evi[
    nzchar(landiq_CLASS) & nzchar(pft),
    .(mx = mean(mx, na.rm = TRUE)),
    by = .(landiq_CLASS, pft)
  ]
  if (!nrow(grp)) {
    return(empty)
  }

  lai_fun <- pool_env$compute_lai_from_mslsp
  if (!is.function(lai_fun)) {
    lai_fun <- get("compute_lai_from_mslsp", inherits = TRUE)
  }
  lai_one <- function(mx, pft) {
    tryCatch(
      as.numeric(lai_fun(mslsp_EVImax = mx, pft = pft)[1]),
      error = function(e) NA_real_
    )
  }
  grp[, lai_fb := mapply(lai_one, mx, pft)]
  grp <- grp[is.finite(lai_fb)]
  if (!nrow(grp)) {
    return(empty)
  }

  pft_fb <- grp[, .(lai_fb = mean(lai_fb, na.rm = TRUE)), by = pft]
  list(
    class_pft = grp[, .(landiq_CLASS, pft, lai_fb)],
    pft = pft_fb,
    global = mean(grp$lai_fb, na.rm = TRUE)
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

# Overlay rows -> LAI via compute_lai_from_mslsp (or CLASS/PFT fallback).
build_planting_lai_table <- function(matched, pool_env) {
  matched <- planting_keep_dated_crop_rows(matched)
  message("[lai] Crop rows with planting date: ", nrow(matched))
  lai_fb <- planting_lai_fallbacks(matched, pool_env)
  message(
    "  LAI fallbacks ready: class-pft=", nrow(lai_fb$class_pft),
    " pft=", nrow(lai_fb$pft),
    " global=", if (is.finite(lai_fb$global)) round(lai_fb$global, 3) else "NA"
  )

  lai_fun <- pool_env$compute_lai_from_mslsp
  if (!is.function(lai_fun)) {
    lai_fun <- get("compute_lai_from_mslsp", inherits = TRUE)
  }

  n <- nrow(matched)
  out <- vector("list", n)
  n_evi <- 0L
  n_fb <- 0L
  n_skip <- 0L
  for (i in seq_len(n)) {
    row <- matched[i]
    code <- paste0(trimws(as.character(row$landiq_CLASS)), as.character(row$landiq_SUBCLASS))
    base <- data.table::data.table(
      parcel_id = as.character(row$parcel_id[1]),
      year = row$year,
      season = row$season,
      date = as.character(row$planting_date_str[1]),
      code = code,
      PFT = as.character(row$landiq_PFT[1]),
      landiq_CLASS = as.character(row$landiq_CLASS[1]),
      landiq_SUBCLASS = as.character(row$landiq_SUBCLASS[1]),
      assigned_by = as.character(row$assigned_by[1]),
      gapfill_date_source = as.character(row$gapfill_date_source[1]),
      mslsp_EVImax = suppressWarnings(as.numeric(row$mslsp_EVImax[1])),
      LAI = NA_real_,
      lai_source = NA_character_,
      lai_k = NA_real_,
      lai_evi_value_used = NA_real_
    )
    mx <- base$mslsp_EVImax[1]
    if (is.finite(mx)) {
      diag <- tryCatch(
        lai_fun(
          mslsp_EVImax = mx,
          pft = base$PFT[1],
          diagnostics = TRUE
        ),
        error = function(e) NULL
      )
      if (is.null(diag) || !is.finite(diag$LAI)) {
        n_skip <- n_skip + 1L
        next
      }
      base$LAI <- as.numeric(diag$LAI)
      base$lai_source <- "mslsp_evi"
      base$lai_k <- as.numeric(diag$lai_k)
      base$lai_evi_value_used <- as.numeric(diag$lai_evi_value_used)
      n_evi <- n_evi + 1L
    } else {
      lai_arg <- lookup_planting_lai_fallback(
        class = base$landiq_CLASS[1],
        pft = base$PFT[1],
        fb = lai_fb
      )
      if (!is.finite(lai_arg)) {
        n_skip <- n_skip + 1L
        next
      }
      base$LAI <- as.numeric(lai_arg)
      base$lai_source <- "lai_fallback"
      n_fb <- n_fb + 1L
    }
    out[[i]] <- base
    if (i %% 10000L == 0L) {
      message("  ", i, "/", n, " done")
    }
  }
  message("  LAI from MSLSP EVImax: ", n_evi)
  message("  LAI from class/PFT fallback: ", n_fb)
  message("  Skipped (no LAI): ", n_skip)
  dt <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  data.table::setorder(dt, parcel_id, year, season)
  dt
}

# LAI table -> C/N via initialize_planting(LAI=). Does not recompute LAI.
build_planting_pool_table <- function(lai_dt, pool_env, lk) {
  n <- nrow(lai_dt)
  message("[pools] Rows: ", n)
  out <- vector("list", n)
  n_ok <- 0L
  n_fail <- 0L
  for (i in seq_len(n)) {
    row <- lai_dt[i]
    lai_source <- as.character(row$lai_source[1])
    p <- NULL
    if (is.finite(as.numeric(row$LAI[1]))) {
      p <- tryCatch(
        pool_env$initialize_planting(
          ID = row$parcel_id[1],
          DATE = as.character(row$date[1]),
          PFT = row$PFT[1],
          lk = lk,
          class = row$landiq_CLASS[1],
          subclass = row$landiq_SUBCLASS[1],
          LAI = as.numeric(row$LAI[1]),
          diagnostics = TRUE
        ),
        error = function(e) NULL
      )
      if (is.null(p) || !nrow(p)) {
        n_fail <- n_fail + 1L
      } else {
        n_ok <- n_ok + 1L
      }
    } else {
      n_fail <- n_fail + 1L
    }
    out[[i]] <- data.table::data.table(
      site_id = as.character(row$parcel_id[1]),
      year = row$year,
      season = row$season,
      date = as.character(row$date[1]),
      code = as.character(row$code[1]),
      PFT = as.character(row$PFT[1]),
      assigned_by = as.character(row$assigned_by[1]),
      gapfill_date_source = as.character(row$gapfill_date_source[1]),
      LAI = as.numeric(row$LAI[1]),
      lai_source = lai_source,
      lai_k = as.numeric(row$lai_k[1]),
      C_LEAF = if (is.null(p)) NA_real_ else as.numeric(p$C_LEAF[1]),
      C_STEM = if (is.null(p)) NA_real_ else as.numeric(p$C_STEM[1]),
      C_FINEROOT = if (is.null(p)) NA_real_ else as.numeric(p$C_FINEROOT[1]),
      C_COARSEROOT = if (is.null(p)) NA_real_ else as.numeric(p$C_COARSEROOT[1]),
      N_LEAF = if (is.null(p)) NA_real_ else as.numeric(p$N_LEAF[1]),
      N_STEM = if (is.null(p)) NA_real_ else as.numeric(p$N_STEM[1]),
      N_FINEROOT = if (is.null(p)) NA_real_ else as.numeric(p$N_FINEROOT[1]),
      N_COARSEROOT = if (is.null(p)) NA_real_ else as.numeric(p$N_COARSEROOT[1]),
      sla_src = if (is.null(p)) NA_character_ else as.character(p$sla_src[1]),
      src_9 = if (is.null(p)) NA_character_ else as.character(p$src_9[1]),
      src_470 = if (is.null(p)) NA_character_ else as.character(p$src_470[1]),
      src_2005 = if (is.null(p)) NA_character_ else as.character(p$src_2005[1]),
      src_1534 = if (is.null(p)) NA_character_ else as.character(p$src_1534[1]),
      src_1019 = if (is.null(p)) NA_character_ else as.character(p$src_1019[1]),
      root_split_src = if (is.null(p)) NA_character_ else as.character(p$root_split_src[1]),
      used_default_split = if (is.null(p)) FALSE else isTRUE(p$used_default_split[1]),
      used_lit_any = if (is.null(p)) FALSE else isTRUE(p$used_lit_any[1]),
      used_pft_any = if (is.null(p)) FALSE else isTRUE(p$used_pft_any[1]),
      used_class_any = if (is.null(p)) FALSE else isTRUE(p$used_class_any[1])
    )
    if (i %% 10000L == 0L) {
      message("  ", i, "/", n, " done")
    }
  }
  message("  Pools from LAI + lookup: ", n_ok)
  message("  Failed / no LAI: ", n_fail)
  dt <- data.table::rbindlist(out, use.names = TRUE, fill = TRUE)
  data.table::setorder(dt, site_id, year, season)
  dt
}
