# Combine management event types into one PEcAn-format JSON (SIPNET)
#
# Merges planting, harvest, tillage, and irrigation tables into a single JSON
# object keyed by site_id. Each site has lists of events by type. Uses
# pool_calculations_from_lookup.R for planting C/N pools and harvest fractions.
# Pass NULL for any event type you do not have.
#
# Main inputs: optional data frames (or CSV paths via CLI): planting, harvest,
# tillage, irrigation. See schemas below.
# Main output: one JSON file (e.g. event_files/<prefix>_events_pecanFormat.json).
# How to run: source() and call combine_management_events_pecan(), or
#   Rscript $CCMMF_CODE/events/combine_management_events_pecan.R --out ... [--planting ...] ...
#
# Contrast: make_events_statewide.R builds yearly statewide phenology + planting
# from assigned MSLSP only. This script is for assembling multiple types from
# arbitrary tables (subsets, extra types, or manual CSVs).

suppressPackageStartupMessages({
  library(dplyr)
  library(jsonlite)
  library(readr)
  library(tibble)
  library(tidyr)
})

# -----------------------------------------------------------------------------
# Input schemas (required columns)
# -----------------------------------------------------------------------------
# planting:   site_id, date, CLASS_SUBCLASS, PFT, and either:
#             (a) LAI
#             (b) mslsp_EVImax + mslsp_EVIamp  (LAI computed in pools script)
# harvest:   site_id, date, CLASS_SUBCLASS, PFT [, destructive]
# tillage:   site_id, date, tillage_eff_0to1
# irrigation: site_id, date, amount_mm, method
# -----------------------------------------------------------------------------

combine_management_events_pecan <- function(planting   = NULL,
                                            harvest    = NULL,
                                            tillage    = NULL,
                                            irrigation = NULL,
                                            out_path   = NULL,
                                            pool_script = NULL) {

  if (is.null(pool_script)) {
    code <- trimws(Sys.getenv("CCMMF_CODE", ""))
    if (nzchar(code)) {
      pool_script <- file.path(code, "traits", "pool_calculations_from_lookup.R")
    } else {
      mgmt <- trimws(Sys.getenv("PRODUCTS_INVENTORY", ""))
      if (!nzchar(mgmt)) {
        root <- trimws(Sys.getenv("CCMMF_ROOT", ""))
        if (nzchar(root)) mgmt <- file.path(root, "products", "inventory")
      }
      if (!nzchar(mgmt)) {
        stop("Set CCMMF_CODE, PRODUCTS_INVENTORY, or CCMMF_ROOT for pool_script.")
      }
      pool_script <- file.path(mgmt, "scripts/traits/pool_calculations_from_lookup.R")
    }
  }
  if (file.exists(pool_script)) {
    pool_env <- new.env(parent = globalenv())
    source(pool_script, local = pool_env)
    lk <- pool_env$load_trait_lookup()
  } else {
    pool_env <- NULL
    lk <- NULL
  }

  # Collect all site_ids
  site_ids <- character(0)
  for (df in list(planting, harvest, tillage, irrigation)) {
    if (!is.null(df) && nrow(df) > 0 && "site_id" %in% names(df)) {
      site_ids <- c(site_ids, as.character(unique(df$site_id)))
    }
  }
  site_ids <- unique(site_ids)
  if (length(site_ids) == 0) {
    stop("No events provided; need at least one of planting, harvest, tillage, irrigation with site_id")
  }

  # Initialize per-site structure
  out <- lapply(site_ids, function(sid) {
    list(tillage = list(), planting = list(), harvest = list(), irrigation = list())
  })
  names(out) <- site_ids

  # ---- Planting ----
  if (!is.null(planting) && nrow(planting) > 0 && !is.null(lk)) {
    stopifnot(all(c("site_id", "date", "CLASS_SUBCLASS", "PFT") %in% names(planting)))
    has_lai <- "LAI" %in% names(planting)
    has_mslsp <- all(c("mslsp_EVImax", "mslsp_EVIamp") %in% names(planting))
    if (!has_lai && !has_mslsp) {
      stop("Planting data must include either LAI, or both mslsp_EVImax and mslsp_EVIamp.")
    }
    for (i in seq_len(nrow(planting))) {
      row <- planting[i, ]
      sid <- as.character(row$site_id)
      code <- as.character(row$CLASS_SUBCLASS)
      pft <- as.character(row$PFT)
      date_str <- as.character(row$date)[1]
      class_val <- if ("CLASS" %in% names(row)) as.character(row$CLASS)[1] else NA_character_

      if (has_lai && !is.na(as.numeric(row$LAI)[1])) {
        lai <- as.numeric(row$LAI)[1]
        p <- pool_env$initialize_planting(
          ID = sid, DATE = date_str, PFT = pft, lk = lk,
          code = code, LAI = lai
        )
      } else if (has_mslsp) {
        p <- pool_env$initialize_planting(
          ID = sid,
          DATE = date_str,
          PFT = pft,
          lk = lk,
          code = code,
          class = class_val,
          mslsp_EVImax = as.numeric(row$mslsp_EVImax)[1],
          mslsp_EVIamp = as.numeric(row$mslsp_EVIamp)[1]
        )
      } else {
        stop("Missing LAI for planting row and no MSLSP EVI columns available.")
      }
      if (!is.null(p) && nrow(p) > 0) {
        evt <- list(
          event_type = "planting",
          date = date_str,
          crop = p$crop_desc[1],
          leaf_c_kg_m2 = as.numeric(p$C_LEAF[1]),
          stem_c_kg_m2 = as.numeric(p$C_STEM[1]),
          fineroot_c_kg_m2 = as.numeric(p$C_FINEROOT[1]),
          coarseroot_c_kg_m2 = as.numeric(p$C_COARSEROOT[1]),
          leaf_n_kg_m2 = as.numeric(p$N_LEAF[1]),
          stem_n_kg_m2 = as.numeric(p$N_STEM[1]),
          fineroot_n_kg_m2 = as.numeric(p$N_FINEROOT[1]),
          coarseroot_n_kg_m2 = as.numeric(p$N_COARSEROOT[1])
        )
        out[[sid]]$planting <- c(out[[sid]]$planting, list(evt))
      }
    }
  }

  # ---- Harvest ----
  if (!is.null(harvest) && nrow(harvest) > 0 && !is.null(lk)) {
    stopifnot(all(c("site_id", "date", "CLASS_SUBCLASS", "PFT") %in% names(harvest)))
    destructive <- if ("destructive" %in% names(harvest)) harvest$destructive else FALSE
    for (i in seq_len(nrow(harvest))) {
      row <- harvest[i, ]
      sid <- as.character(row$site_id)
      code <- as.character(row$CLASS_SUBCLASS)
      pft <- as.character(row$PFT)
      date_str <- as.character(row$date)[1]
      dest <- if (length(destructive) >= i) as.logical(destructive[i])[1] else FALSE

      h <- pool_env$initialize_harvest_from_lookup(
        ID = sid, DATE = date_str, code = code, PFT = pft, lk = lk, destructive = dest
      )
      if (!is.null(h) && nrow(h) > 0) {
        evt <- list(
          event_type = "harvest",
          date = date_str,
          crop = h$crop_desc[1],
          frac_above_removed_0to1 = as.numeric(h$AGB_REMOVED[1]),
          frac_above_to_litter_0to1 = as.numeric(h$AGB_LITTER[1]),
          frac_below_removed_0to1 = as.numeric(h$BGB_REMOVED[1]),
          frac_below_to_litter_0to1 = as.numeric(h$BGB_LITTER[1])
        )
        out[[sid]]$harvest <- c(out[[sid]]$harvest, list(evt))
      }
    }
  }

  # ---- Tillage ----
  if (!is.null(tillage) && nrow(tillage) > 0) {
    stopifnot(all(c("site_id", "date", "tillage_eff_0to1") %in% names(tillage)))
    for (i in seq_len(nrow(tillage))) {
      row <- tillage[i, ]
      sid <- as.character(row$site_id)
      evt <- list(
        event_type = "tillage",
        date = as.character(row$date)[1],
        tillage_eff_0to1 = as.numeric(row$tillage_eff_0to1)[1]
      )
      out[[sid]]$tillage <- c(out[[sid]]$tillage, list(evt))
    }
  }

  # ---- Irrigation ----
  if (!is.null(irrigation) && nrow(irrigation) > 0) {
    stopifnot(all(c("site_id", "date", "amount_mm", "method") %in% names(irrigation)))
    for (i in seq_len(nrow(irrigation))) {
      row <- irrigation[i, ]
      sid <- as.character(row$site_id)
      method_val <- row$method[1]
      if (is.numeric(method_val)) {
        method_str <- switch(as.character(method_val), "1" = "soil", "2" = "spray", "3" = "drip", as.character(method_val))
      } else {
        method_str <- as.character(method_val)
      }
      evt <- list(
        event_type = "irrigation",
        date = as.character(row$date)[1],
        amount_mm = as.numeric(row$amount_mm)[1],
        method = method_str
      )
      out[[sid]]$irrigation <- c(out[[sid]]$irrigation, list(evt))
    }
  }

  # Sort events by date within each site
  for (sid in names(out)) {
    for (evt_type in c("tillage", "planting", "harvest", "irrigation")) {
      L <- out[[sid]][[evt_type]]
      if (length(L) > 0) {
        dates <- vapply(L, function(x) x$date, character(1))
        out[[sid]][[evt_type]] <- L[order(dates)]
      }
    }
  }

  if (!is.null(out_path)) {
    dir.create(dirname(out_path), recursive = TRUE, showWarnings = FALSE)
    write(jsonlite::toJSON(out, auto_unbox = TRUE, pretty = TRUE), out_path)
    message("Wrote PEcAn events: ", out_path)
    return(invisible(out))
  }

  out
}

# -----------------------------------------------------------------------------
# CLI: parse args and run
# -----------------------------------------------------------------------------
.args <- commandArgs(trailingOnly = TRUE)
if (length(.args) > 0) {
  .i <- 1
  .planting <- .harvest <- .tillage <- .irrigation <- .out <- NULL
  while (.i <= length(.args)) {
    if (.args[.i] == "--planting" && .i < length(.args)) { .planting <- .args[.i + 1]; .i <- .i + 2; next }
    if (.args[.i] == "--harvest"  && .i < length(.args)) { .harvest  <- .args[.i + 1]; .i <- .i + 2; next }
    if (.args[.i] == "--tillage"  && .i < length(.args)) { .tillage  <- .args[.i + 1]; .i <- .i + 2; next }
    if (.args[.i] == "--irrigation" && .i < length(.args)) { .irrigation <- .args[.i + 1]; .i <- .i + 2; next }
    if (.args[.i] == "--out"      && .i < length(.args)) { .out      <- .args[.i + 1]; .i <- .i + 2; next }
    .i <- .i + 1
  }

  .p <- .h <- .t <- .ir <- NULL
  if (!is.null(.planting)   && file.exists(.planting))   .p <- read_csv(.planting, show_col_types = FALSE)
  if (!is.null(.harvest)    && file.exists(.harvest))    .h <- read_csv(.harvest,  show_col_types = FALSE)
  if (!is.null(.tillage)    && file.exists(.tillage))    .t <- read_csv(.tillage,  show_col_types = FALSE)
  if (!is.null(.irrigation) && file.exists(.irrigation)) .ir <- read_csv(.irrigation, show_col_types = FALSE)

  if (is.null(.out)) stop("--out path required")
  combine_management_events_pecan(planting = .p, harvest = .h, tillage = .t, irrigation = .ir, out_path = .out)
}
