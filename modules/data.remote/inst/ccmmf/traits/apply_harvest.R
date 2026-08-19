#!/usr/bin/env Rscript
# Overlay + harvest lookup -> rem/lit table (initialize_harvest_from_lookup).
# Includes CLASS-level woody clearing look-ahead.
# Writes $MATCHED_DIR/assigned_year=Y_harvest.parquet
# Math: pool_calculations_from_lookup.R (via trait pool).
# Usage: Rscript apply_harvest.R <year>

harvest_empty_table <- function() {
  data.table::data.table(
    site_id = character(),
    year = integer(),
    season = integer(),
    date = character(),
    CLASS_SUBCLASS = character(),
    PFT = character(),
    assigned_by = character(),
    gapfill_date_source = character(),
    destructive = logical(),
    frac_above_removed_0to1 = numeric(),
    frac_above_to_litter_0to1 = numeric(),
    frac_below_removed_0to1 = numeric(),
    frac_below_to_litter_0to1 = numeric(),
    lookup_pft = character(),
    src_agb_removed = character(),
    src_agb_litter = character(),
    src_bgb_removed = character(),
    src_bgb_litter = character()
  )
}

# Overlay rows -> rem/lit via initialize_harvest_from_lookup (plus woody clearing).
build_harvest_fraction_table <- function(matched, year, pool_env, lk, paths,
                                         destructive_default = harvest_destructive_default()) {
  message("[harvest] Building fractions table (lookup rem/lit)")
  if (!"harvest_date_str" %in% names(matched)) {
    matched[, pft_l := tolower(trimws(as.character(landiq_PFT)))]
    matched[, harvest_date_str := NA_character_]
    # Annuals: OGMn. Hay/woody routine harvest: OGD. Woody destructive: OGMn
    # (look-ahead below).
    matched[pft_l %in% c("row", "rice"), harvest_date_str := as.character(mslsp_OGMn)]
    matched[pft_l %in% c("hay", "woody"), harvest_date_str := as.character(mslsp_OGD)]
    matched[, pft_l := NULL]
  }
  n0 <- nrow(matched)
  matched <- matched[
    !is.na(harvest_date_str) & nzchar(as.character(harvest_date_str)) &
      as.character(harvest_date_str) != "NA"
  ]
  message("  Rows with harvest date: ", nrow(matched), " (from ", n0, ")")

  if ("landiq_PFT" %in% names(matched)) {
    is_other <- tolower(trimws(as.character(matched$landiq_PFT))) == "other"
    n_skip_other <- sum(is_other, na.rm = TRUE)
    if (n_skip_other > 0L) {
      matched <- matched[!is_other]
      message("  Skipped harvest (PFT other / idle-fallow): ", n_skip_other)
    }
  }

  has_dest_col <- "destructive" %in% names(matched)
  has_specond_col <- "landiq_SPECOND" %in% names(matched)
  if (!has_specond_col) {
    message(
      "  Note: assigned parquet has no landiq_SPECOND; young-woody skip uses CLASS=YP only ",
      "(re-run match for SPECOND=Y on D/C)."
    )
  }

  harvest_rows <- vector("list", nrow(matched))
  n_skip_young_woody <- 0L
  for (i in seq_len(nrow(matched))) {
    row <- matched[i]
    specond <- if (has_specond_col) row$landiq_SPECOND[1] else NA_character_
    if (is_young_woody_harvest(row$landiq_PFT[1], row$landiq_CLASS[1], specond)) {
      n_skip_young_woody <- n_skip_young_woody + 1L
      next
    }
    code <- paste0(trimws(as.character(row$landiq_CLASS)), as.character(row$landiq_SUBCLASS))
    dest <- if (has_dest_col) isTRUE(as.logical(row$destructive[1])) else destructive_default
    h <- tryCatch(
      pool_env$initialize_harvest_from_lookup(
        ID = row$parcel_id,
        DATE = as.character(row$harvest_date_str)[1],
        code = code,
        PFT = row$landiq_PFT,
        lk = lk,
        destructive = dest,
        diagnostics = TRUE
      ),
      error = function(e) NULL
    )
    if (!is.null(h) && nrow(h) > 0) {
      harvest_rows[[i]] <- data.table::data.table(
        site_id = row$parcel_id,
        year = row$year,
        season = row$season,
        date = as.character(row$harvest_date_str)[1],
        CLASS_SUBCLASS = code,
        PFT = row$landiq_PFT,
        assigned_by = as.character(row$assigned_by[1]),
        gapfill_date_source = as.character(row$gapfill_date_source[1]),
        destructive = isTRUE(dest),
        frac_above_removed_0to1 = as.numeric(h$AGB_REMOVED[1]),
        frac_above_to_litter_0to1 = as.numeric(h$AGB_LITTER[1]),
        frac_below_removed_0to1 = as.numeric(h$BGB_REMOVED[1]),
        frac_below_to_litter_0to1 = as.numeric(h$BGB_LITTER[1]),
        lookup_pft = as.character(h$lookup_pft[1]),
        src_agb_removed = as.character(h$src_agb_removed[1]),
        src_agb_litter = as.character(h$src_agb_litter[1]),
        src_bgb_removed = as.character(h$src_bgb_removed[1]),
        src_bgb_litter = as.character(h$src_bgb_litter[1])
      )
    }
    if (i %% 10000L == 0L) {
      message("  ", i, "/", nrow(matched), " done")
    }
  }

  harvest_dt <- data.table::rbindlist(harvest_rows, use.names = TRUE, fill = TRUE)
  message("  Skipped young woody harvest (SPECOND=Y or CLASS=YP): ", n_skip_young_woody)

  dest_dt <- build_woody_destructive_from_transition(
    year = year,
    matched = matched,
    pool_env = pool_env,
    lk = lk,
    paths = paths
  )
  if (!is.null(dest_dt) && nrow(dest_dt) > 0L) {
    dest_sites <- unique(as.character(dest_dt$site_id))
    n_drop <- 0L
    if (nrow(harvest_dt) > 0L) {
      drop_idx <- tolower(trimws(as.character(harvest_dt$PFT))) == "woody" &
        as.character(harvest_dt$site_id) %in% dest_sites
      n_drop <- sum(drop_idx, na.rm = TRUE)
      harvest_dt <- harvest_dt[!drop_idx]
    }
    message(
      "  Replaced ", n_drop, " routine woody harvest row(s) with ",
      nrow(dest_dt), " destructive event(s) (stand removal)"
    )
    harvest_dt <- data.table::rbindlist(list(harvest_dt, dest_dt), use.names = TRUE, fill = TRUE)
  }

  if (nrow(harvest_dt) == 0L) {
    message("  No harvest rows for year=", year)
    return(harvest_empty_table())
  }
  if (!"destructive" %in% names(harvest_dt)) {
    harvest_dt[, destructive := FALSE]
  }
  harvest_dt[is.na(destructive), destructive := FALSE]
  data.table::setorder(harvest_dt, site_id, year, season, destructive)
  harvest_dt
}

# Look ahead year -> year+1 on LandIQ season 2. When a mature woody CLASS is replaced
# (different CLASS, young woody, or non-woody), emit one destructive harvest row
# (PFT=woody + destructive=TRUE clearing fractions)
# using the prior stand's crop code (caller drops any routine woody harvest for
# those parcels). Subclass-only changes do not fire.
build_woody_destructive_from_transition <- function(year, matched, pool_env, lk, paths) {
  yr <- as.integer(year)
  next_yr <- yr + 1L
  message(
    "[harvest] Woody destructive look-ahead: LandIQ season 2 ",
    yr, " -> ", next_yr, " (CLASS-level)"
  )

  prior <- load_landiq_season2_identity(yr, paths$landiq_crops, paths$cropcode_csv)
  curr <- load_landiq_season2_identity(next_yr, paths$landiq_crops, paths$cropcode_csv)
  if (is.null(prior) || is.null(curr)) {
    message(
      "  Skip woody destructive: need season-2 LandIQ for both ",
      yr, " and ", next_yr, " (re-run prior year after new LandIQ year exists)."
    )
    return(NULL)
  }

  data.table::setnames(
    prior,
    c("CLASS", "SUBCLASS", "PFT", "SPECOND"),
    c("prior_CLASS", "prior_SUBCLASS", "prior_PFT", "prior_SPECOND")
  )
  data.table::setnames(
    curr,
    c("CLASS", "SUBCLASS", "PFT", "SPECOND"),
    c("curr_CLASS", "curr_SUBCLASS", "curr_PFT", "curr_SPECOND")
  )

  trans <- merge(prior, curr, by = "parcel_id", all.x = TRUE)
  prior_mature <- tolower(trimws(as.character(trans$prior_PFT))) == "woody" &
    !(toupper(trimws(as.character(trans$prior_CLASS))) == "YP" |
      toupper(trimws(as.character(trans$prior_SPECOND))) == "Y")
  curr_class <- toupper(trimws(as.character(trans$curr_CLASS)))
  curr_missing <- is.na(trans$curr_CLASS) | !nzchar(trimws(as.character(trans$curr_CLASS)))
  curr_mature <- !curr_missing &
    tolower(trimws(as.character(trans$curr_PFT))) == "woody" &
    !(curr_class == "YP" |
      toupper(trimws(as.character(trans$curr_SPECOND))) == "Y")
  class_changed <- !curr_missing &
    toupper(trimws(as.character(trans$prior_CLASS))) != curr_class
  trans[, destroy := prior_mature & (curr_missing | !curr_mature | class_changed)]
  trans <- trans[destroy == TRUE]
  message("  Woody CLASS transitions (destructive): ", nrow(trans))
  if (nrow(trans) == 0L) {
    return(NULL)
  }

  date_by_parcel <- NULL
  if ("harvest_date_str" %in% names(matched) && nrow(matched) > 0L) {
    mw <- matched[
      tolower(trimws(as.character(landiq_PFT))) == "woody",
      .(
        date = as.character(mslsp_OGMn)[1L],
        season = as.integer(season)[1L],
        assigned_by = as.character(assigned_by)[1L],
        gapfill_date_source = as.character(gapfill_date_source)[1L]
      ),
      by = parcel_id
    ]
    date_by_parcel <- mw
  }
  if (is.null(date_by_parcel)) {
    date_by_parcel <- data.table::data.table(
      parcel_id = character(),
      date = character(),
      season = integer(),
      assigned_by = character(),
      gapfill_date_source = character()
    )
  }

  trans <- merge(trans, date_by_parcel, by = "parcel_id", all.x = TRUE)
  fallback_date <- sprintf("%d-12-31", yr)
  trans[is.na(date) | !nzchar(date) | date == "NA", date := fallback_date]
  trans[is.na(season), season := 2L]

  dest_rows <- vector("list", nrow(trans))
  n_ok <- 0L
  for (i in seq_len(nrow(trans))) {
    row <- trans[i]
    code <- paste0(
      trimws(as.character(row$prior_CLASS)),
      as.character(row$prior_SUBCLASS)
    )
    h <- tryCatch(
      pool_env$initialize_harvest_from_lookup(
        ID = row$parcel_id,
        DATE = row$date,
        code = code,
        PFT = "woody",
        lk = lk,
        destructive = TRUE,
        diagnostics = TRUE
      ),
      error = function(e) NULL
    )
    if (!is.null(h) && nrow(h) > 0) {
      n_ok <- n_ok + 1L
      dest_rows[[i]] <- data.table::data.table(
        site_id = row$parcel_id,
        year = yr,
        season = as.integer(row$season),
        date = as.character(row$date),
        CLASS_SUBCLASS = code,
        PFT = "woody",
        assigned_by = as.character(row$assigned_by[1]),
        gapfill_date_source = as.character(row$gapfill_date_source[1]),
        destructive = TRUE,
        frac_above_removed_0to1 = as.numeric(h$AGB_REMOVED[1]),
        frac_above_to_litter_0to1 = as.numeric(h$AGB_LITTER[1]),
        frac_below_removed_0to1 = as.numeric(h$BGB_REMOVED[1]),
        frac_below_to_litter_0to1 = as.numeric(h$BGB_LITTER[1]),
        lookup_pft = as.character(h$lookup_pft[1]),
        src_agb_removed = as.character(h$src_agb_removed[1]),
        src_agb_litter = as.character(h$src_agb_litter[1]),
        src_bgb_removed = as.character(h$src_bgb_removed[1]),
        src_bgb_litter = as.character(h$src_bgb_litter[1])
      )
    }
  }
  message("  Destructive harvest rows written: ", n_ok)
  data.table::rbindlist(dest_rows, use.names = TRUE, fill = TRUE)
}

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
})

args <- commandArgs(trailingOnly = TRUE)
if (!length(args)) {
  stop("Usage: Rscript apply_harvest.R <year>")
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
source(file.path(events_root, "R", "matched_input.R"))
source(file.path(events_root, "R", "trait_pool.R"))

paths <- events_paths()
pool <- load_events_trait_pool(paths$pool_script)
matched <- load_matched_for_events(
  year_arg, paths$matched_dir, run_harvest = TRUE
)
harvest_dt <- build_harvest_fraction_table(
  matched, year_arg, pool$pool_env, pool$lk, paths
)
out <- harvest_table_path(paths$matched_dir, year_arg)
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
arrow::write_parquet(harvest_dt, out)
message("[harvest] wrote ", nrow(harvest_dt), " rows: ", out)
if (nrow(harvest_dt)) {
  print(harvest_dt[, .N, by = .(PFT, destructive)])
}
