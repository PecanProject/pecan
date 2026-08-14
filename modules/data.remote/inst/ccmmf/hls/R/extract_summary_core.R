# =============================================================================
# extract_summary_core.R — weighted summary helpers for HLS extraction
# =============================================================================
# Used by MSLSP (and similar products) for per-polygon weighted mean/sd and
# QA mode. n_eff = w_valid^2 / sum_w2 for edge-corrected SE; na_frac = data quality.
# =============================================================================

suppressPackageStartupMessages(library(data.table))

# --- Weighted statistics ---
# Returns mean, sd, n_valid, w_valid, sum_w2, na_frac for SE and quality flags.
weighted_stats <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) {
    return(list(mean = NA_real_, sd = NA_real_, n_valid = 0L,
                w_valid = 0, sum_w2 = 0, na_frac = NA_real_))
  }
  x2 <- as.numeric(x[ok])
  w2 <- as.numeric(w[ok])
  wsum <- sum(w2)
  mu <- sum(w2 * x2) / wsum
  sd <- sqrt(sum(w2 * (x2 - mu)^2) / wsum)

  w_all <- w[!is.na(w) & w > 0]
  w_na  <- w[is.na(x) & !is.na(w) & w > 0]
  na_frac <- if (length(w_all) == 0) NA_real_ else sum(w_na, na.rm = TRUE) / sum(w_all, na.rm = TRUE)

  list(
    mean    = as.numeric(mu),
    sd      = as.numeric(sd),
    n_valid = as.integer(length(x2)),
    w_valid = as.numeric(wsum),
    sum_w2  = as.numeric(sum(w2^2)),
    na_frac = as.numeric(na_frac)
  )
}

weighted_mode_stats <- function(x, w) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) return(list(mode = NA_real_, mode_frac = NA_real_))
  dt <- data.table(x = x[ok], w = w[ok])
  agg <- dt[, .(w_sum = sum(w)), by = x][order(-w_sum)]
  mode_val <- agg$x[1]
  mode_frac <- agg$w_sum[1] / sum(agg$w_sum)
  list(mode = as.numeric(mode_val), mode_frac = as.numeric(mode_frac))
}

# One row per polygon: coverage stats, weighted mean/sd per continuous layer, QA mode per QA layer.
summarize_extract <- function(ex_dt, id_vec, layer_names, qa_names = character(), id_col = "parcel_id") {
  stopifnot("ID" %in% names(ex_dt), "weight" %in% names(ex_dt))
  stopifnot(length(id_vec) >= max(ex_dt$ID, na.rm = TRUE))

  dt <- as.data.table(ex_dt)
  dt[, (id_col) := id_vec[ID]]
  wcol <- "weight"

  cont_layers <- setdiff(layer_names, qa_names)
  if (length(cont_layers) == 0) stop("No continuous layers to compute coverage stats.")
  coverage_layer <- cont_layers[1]

  sup <- dt[, {
    s <- weighted_stats(get(coverage_layer), get(wcol))
    list(n_valid = as.integer(s$n_valid), w_valid = as.numeric(s$w_valid),
         sum_w2 = as.numeric(s$sum_w2), na_frac = as.numeric(s$na_frac))
  }, by = c(id_col)]

  cont_out <- dt[, {
    out <- list()
    for (nm in cont_layers) {
      s <- weighted_stats(get(nm), get(wcol))
      out[[paste0(nm, "_mean")]] <- as.numeric(s$mean)
      out[[paste0(nm, "_sd")]] <- as.numeric(s$sd)
    }
    out
  }, by = c(id_col)]

  qa_out <- NULL
  if (length(qa_names) > 0) {
    qa_out <- dt[, {
      out <- list()
      for (q in qa_names) {
        m <- weighted_mode_stats(get(q), get(wcol))
        out[[paste0(q, "_mode")]] <- as.numeric(m$mode)
        out[[paste0(q, "_mode_frac")]] <- as.numeric(m$mode_frac)
      }
      out
    }, by = c(id_col)]
  }

  res <- merge(sup, cont_out, by = id_col, all = TRUE)
  if (!is.null(qa_out)) res <- merge(res, qa_out, by = id_col, all = TRUE)
  res[]
}

