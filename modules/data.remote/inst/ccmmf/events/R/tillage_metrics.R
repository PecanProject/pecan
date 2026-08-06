# tillage_metrics(ndti_table, phenology_table)
#
# NDTI: parcel_id, year, date, ndti_mean, ndti_sd, n_valid, PFT (join PFT from
# assigned before calling if your NDTI table does not have it).
#
# Phenology: parcel_id, year, OGI_date, OGMn_date (Date). Include all cycles /
# seasons per parcel across years so lead(OGI_date) can span cross-year fallow.
# Optional provenance on phenology rows (carried through when present):
# assigned_by, gapfill_date_source -> output as ogmn_*/ogi_* from fallow start
# and lead(OGI).
#
# Returns a tibble of per-fallow-window tillage metrics (dplyr).

# Neighbor NDTI obs (n_valid > 0) before/after target_date; pooled SD when
# target day has no obs. Vectorized over rows of q (parcel_id, target_date).
.min_ndti_neighbor_details <- function(rel_dt, q_dt) {
  n <- nrow(q_dt)
  prev_date <- as.Date(rep(NA_real_, n))
  prev_nv <- rep(NA_real_, n)
  prev_ss <- rep(NA_real_, n)
  foll_date <- as.Date(rep(NA_real_, n))
  foll_nv <- rep(NA_real_, n)
  foll_ss <- rep(NA_real_, n)

  for (pid in unique(q_dt$parcel_id)) {
    ix <- which(q_dt$parcel_id == pid)
    rp <- rel_dt[rel_dt$parcel_id == pid, , drop = FALSE]
    if (nrow(rp) == 0L) {
      next
    }
    o <- order(rp$date)
    rp <- rp[o, , drop = FALSE]
    di <- as.integer(rp$date)
    ti <- as.integer(q_dt$target_date[ix])

    ip <- findInterval(ti - 1L, di)
    hit <- ip >= 1L
    if (any(hit)) {
      ii <- ix[hit]
      jj <- ip[hit]
      prev_date[ii] <- rp$date[jj]
      prev_nv[ii] <- rp$n_valid[jj]
      prev_ss[ii] <- rp$ss[jj]
    }

    fi <- findInterval(ti, di) + 1L
    ok <- fi <= length(di) & di[fi] > ti
    if (any(ok, na.rm = TRUE)) {
      ii <- ix[ok]
      jj <- fi[ok]
      foll_date[ii] <- rp$date[jj]
      foll_nv[ii] <- rp$n_valid[jj]
      foll_ss[ii] <- rp$ss[jj]
    }
  }

  list(
    prev_date = prev_date,
    prev_nv = prev_nv,
    prev_ss = prev_ss,
    foll_date = foll_date,
    foll_nv = foll_nv,
    foll_ss = foll_ss
  )
}

tillage_metrics <- function(ndti_table, phenology_table) {
  ndti_work <- dplyr::as_tibble(ndti_table) |>
    dplyr::mutate(
      date = as.Date(date),
      ss = ifelse(!is.na(n_valid) & n_valid > 0, n_valid * (ndti_sd^2), 0)
    )

  pheno_cols <- c("parcel_id", "year", "OGI_date", "OGMn_date")
  prov_cols <- intersect(
    c("assigned_by", "gapfill_date_source"),
    names(phenology_table)
  )
  pheno_date <- dplyr::as_tibble(phenology_table) |>
    dplyr::select(dplyr::all_of(c(pheno_cols, prov_cols))) |>
    dplyr::mutate(
      OGI_date = as.Date(OGI_date),
      OGMn_date = as.Date(OGMn_date)
    )

  ndti_smooth <- ndti_work |>
    dplyr::inner_join(
      dplyr::select(pheno_date, parcel_id, year, OGI_date, OGMn_date),
      by = c("parcel_id", "year")
    ) |>
    dplyr::arrange(parcel_id, date) |>
    dplyr::group_by(parcel_id, year, PFT) |>
    tidyr::complete(date = seq.Date(min(date), max(date), by = "day")) |>
    tidyr::fill(OGMn_date, OGI_date, .direction = "downup") |>
    dplyr::mutate(
      mean_ndti_filled = zoo::na.approx(ndti_mean, x = date, na.rm = FALSE),
      smoothed = as.numeric(stats::filter(mean_ndti_filled, rep(1 / 4, 4), sides = 2))
    ) |>
    dplyr::ungroup()

  fallow_periods <- pheno_date |>
    dplyr::arrange(parcel_id, OGI_date) |>
    dplyr::group_by(parcel_id) |>
    dplyr::mutate(
      fallow_start = OGMn_date,
      fallow_end = dplyr::lead(OGI_date),
      ogmn_assigned_by = if ("assigned_by" %in% names(pheno_date)) {
        assigned_by
      } else {
        NA_character_
      },
      ogmn_source = if ("gapfill_date_source" %in% names(pheno_date)) {
        gapfill_date_source
      } else {
        NA_character_
      },
      ogi_assigned_by = if ("assigned_by" %in% names(pheno_date)) {
        dplyr::lead(assigned_by)
      } else {
        NA_character_
      },
      ogi_source = if ("gapfill_date_source" %in% names(pheno_date)) {
        dplyr::lead(gapfill_date_source)
      } else {
        NA_character_
      }
    ) |>
    dplyr::filter(!is.na(fallow_end)) |>
    dplyr::ungroup()

  joined_fb <- dplyr::inner_join(
    ndti_smooth,
    fallow_periods,
    by = "parcel_id",
    suffix = c("_ndti", "_fallow")
  )
  yr_col <- if ("year_fallow" %in% names(joined_fb)) "year_fallow" else "year"

  base_metrics <- joined_fb |>
    dplyr::filter(date >= fallow_start, date <= fallow_end) |>
    dplyr::group_by(parcel_id, fallow_start) |>
    dplyr::summarize(
      min_idx = which.min(smoothed),
      minNDTI_date = date[min_idx],
      ndti_on_minNDTI = smoothed[min_idx],
      max_pre_idx = which.max(ifelse(date <= minNDTI_date, smoothed, -Inf)),
      maxNDTI_pre_date = date[max_pre_idx],
      maxNDTI_pre_min = smoothed[max_pre_idx],
      ndti_pct_change = ((maxNDTI_pre_min - ndti_on_minNDTI) / maxNDTI_pre_min) * 100,
      year = dplyr::first(.data[[yr_col]]),
      PFT = dplyr::first(PFT),
      OGMn_date = dplyr::first(fallow_start),
      ogmn_assigned_by = dplyr::first(ogmn_assigned_by),
      ogmn_source = dplyr::first(ogmn_source),
      ogi_assigned_by = dplyr::first(ogi_assigned_by),
      ogi_source = dplyr::first(ogi_source),
      .groups = "drop"
    )

  # One row per fallow window: min-day SD / neighbor counts (was rowwise +
  # get_metric_details per row; res_max branch was never selected -- dropped).
  smooth <- dplyr::distinct(dplyr::as_tibble(ndti_smooth), parcel_id, date, .keep_all = TRUE)
  rel <- dplyr::filter(smooth, !is.na(n_valid), n_valid > 0)

  q <- data.frame(
    parcel_id = base_metrics$parcel_id,
    target_date = as.Date(base_metrics$minNDTI_date),
    stringsAsFactors = FALSE
  )
  row_t <- match(
    paste(q$parcel_id, as.integer(q$target_date), sep = "\r"),
    paste(smooth$parcel_id, as.integer(smooth$date), sep = "\r")
  )
  n_on <- rep(0L, nrow(q))
  okm <- !is.na(row_t)
  if (any(okm)) {
    nvv <- smooth$n_valid[row_t[okm]]
    n_on[okm] <- as.integer(ifelse(is.na(nvv), 0L, nvv))
  }
  tgt_sd <- rep(NA_real_, nrow(q))
  tgt_sd[okm] <- smooth$ndti_sd[row_t[okm]]

  nb <- .min_ndti_neighbor_details(rel, q)
  sd_out <- ifelse(n_on > 0, tgt_sd, NA_real_)
  w0 <- which(n_on == 0L)
  if (length(w0) > 0L) {
    nv <- ifelse(is.na(nb$prev_nv[w0]), 0, nb$prev_nv[w0]) +
      ifelse(is.na(nb$foll_nv[w0]), 0, nb$foll_nv[w0])
    ss <- ifelse(is.na(nb$prev_ss[w0]), 0, nb$prev_ss[w0]) +
      ifelse(is.na(nb$foll_ss[w0]), 0, nb$foll_ss[w0])
    pool <- sqrt(ss / nv)
    pool[nv <= 0 | !is.finite(pool)] <- NA_real_
    sd_out[w0] <- pool
  }

  dplyr::transmute(
    base_metrics,
    parcel_id,
    year,
    PFT,
    OGMn_date,
    max_date = maxNDTI_pre_date,
    max_ndti = maxNDTI_pre_min,
    min_date = minNDTI_date,
    min_ndti = ndti_on_minNDTI,
    min_n_valid = n_on,
    min_sd = sd_out,
    ndti_pct_change,
    min_val_date_before = nb$prev_date,
    min_val_n_before = nb$prev_nv,
    min_val_date_after = nb$foll_date,
    min_val_n_after = nb$foll_nv,
    ogmn_assigned_by,
    ogmn_source,
    ogi_assigned_by,
    ogi_source
  )
}
