#!/usr/bin/env Rscript
# Build statewide PEcAn-style event files for one year (Parquet + JSON) from matched LandIQ+MSLSP.
# Writes phenology, planting, harvest, and optionally tillage event tables under event_files/.
#
# Main inputs: CCMMF_MANAGEMENT (paths to matched assigned_year=Y.parquet, scripts/traits pool,
#   tillage/ndti_v4.1). Optional: HARVEST_LOOKUP_RDS, HARVEST_WOODY_DESTRUCTIVE, TILLAGE_* env.
# Main outputs: phenology_statewide_Y, planting_statewide_Y, harvest_statewide_Y, tillage_statewide_*.
# How to run: Rscript make_events_statewide.R <year> [phenology|planting|harvest|tillage]
# Workflow: end of monitoring chart (statewide outputs). Phenology/planting/harvest read assigned
#   parquet and pool_calculations_from_lookup.R. Harvest dates: row/rice use mslsp_OGMn; hay/woody
#   use mslsp_OGD. Tillage uses NDTI plus assigned parcels in a buffered year window.

suppressPackageStartupMessages({
  library(data.table)
  library(arrow)
  library(jsonlite)
  library(lubridate)
})

path_management <- Sys.getenv("CCMMF_MANAGEMENT", "/projectnb/dietzelab/ccmmf/management")
matched_dir <- file.path(path_management, "phenology", "matched_landiq_mslsp_v4.1")
pool_script <- file.path(path_management, "scripts", "traits", "pool_calculations_from_lookup.R")
tillage_metrics_script <- file.path(path_management, "scripts", "tillage", "tillage_metrics.R")
ndti_root <- file.path(path_management, "tillage", "ndti_v4.1")
out_dir <- file.path(path_management, "event_files")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("Usage: Rscript make_events_statewide.R <year> [phenology|planting|harvest|tillage]")
}
year_arg <- as.integer(args[1L])
if (is.na(year_arg)) {
  stop("Year must be an integer, got: ", args[1L])
}
event_type <- if (length(args) >= 2L) {
  match.arg(args[2L], c("phenology", "planting", "harvest", "tillage"))
} else {
  NULL
}

run_phenology <- is.null(event_type) || event_type == "phenology"
run_planting <- is.null(event_type) || event_type == "planting"
run_harvest <- is.null(event_type) || event_type == "harvest"
run_tillage <- !is.null(event_type) && event_type == "tillage"

msg_suffix <- if (is.null(event_type)) {
  " (phenology + planting + harvest)"
} else {
  paste0(" event_type=", event_type)
}
message("[make_events_statewide] year=", year_arg, msg_suffix)

dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

#### Single-year assigned (phenology, planting, harvest)
if (run_phenology || run_planting || run_harvest) {
  assigned_file <- file.path(matched_dir, sprintf("assigned_year=%d.parquet", year_arg))
  if (!file.exists(assigned_file)) {
    stop("Missing assigned file: ", assigned_file)
  }
  assigned <- as.data.table(read_parquet(assigned_file))
  assigned[, parcel_id := as.character(parcel_id)]
  matched <- assigned[assigned_by == "matched"]
  message("[assigned] ", nrow(assigned), " rows; matched: ", nrow(matched))

  n_before <- nrow(matched)
  matched <- matched[
    !is.na(landiq_CLASS) & !is.na(landiq_SUBCLASS) & !is.na(landiq_PFT)
  ]
  message("  Dropped ", n_before - nrow(matched), " matched rows missing crop/PFT; ", nrow(matched), " remain")

  if (run_phenology || run_planting) {
    n_ev <- nrow(matched)
    matched <- matched[!is.na(mslsp_EVImax) & !is.na(mslsp_EVIamp)]
    message("  EVI filter: dropped ", n_ev - nrow(matched), " rows; ", nrow(matched), " remain")
  }

  if (run_harvest) {
    for (col in c("mslsp_OGMn", "mslsp_OGD")) {
      if (!col %in% names(matched)) {
        matched[, (col) := NA]
      }
    }
    matched[, pft_l := tolower(trimws(as.character(landiq_PFT)))]
    matched[, harvest_date_str := NA_character_]
    matched[pft_l %in% c("row", "rice"), harvest_date_str := as.character(mslsp_OGMn)]
    matched[pft_l %in% c("hay", "woody"), harvest_date_str := as.character(mslsp_OGD)]
    n_hd <- nrow(matched)
    matched <- matched[
      !is.na(harvest_date_str) & nzchar(harvest_date_str) & harvest_date_str != "NA"
    ]
    message(
      "  Harvest-date filter (annuals OGMn, perennials OGD; no Peak): dropped ",
      n_hd - nrow(matched), " rows; ", nrow(matched), " remain"
    )
    matched[, pft_l := NULL]
  }
}

if (run_planting || run_harvest) {
  pool_env <- new.env(parent = globalenv())
  source(pool_script, local = pool_env)
  harvest_rds <- Sys.getenv("HARVEST_LOOKUP_RDS", "")
  if (nzchar(harvest_rds)) {
    lk <- pool_env$load_trait_lookup(harvest_path = harvest_rds)
    message("[pool] Loaded trait lookup (harvest_path=", harvest_rds, ")")
  } else {
    lk <- pool_env$load_trait_lookup()
    message("[pool] Loaded trait lookup (default planting + harvest RDS paths)")
  }
}

harvest_destructive_default <- tolower(Sys.getenv("HARVEST_WOODY_DESTRUCTIVE", "0")) %in% c("1", "true", "yes")

#### PHENOLOGY
if (run_phenology) {
  message("[phenology] Building events")
  pheno <- matched[, .(
    site_id = parcel_id,
    year = lubridate::year(mslsp_Peak),
    leafonday = as.character(mslsp_50PCGI),
    leafoffday = as.character(mslsp_50PCGD)
  )]
  pheno <- pheno[!is.na(leafonday) & !is.na(leafoffday)]
  setorder(pheno, site_id, year)

  pheno_parquet <- file.path(out_dir, sprintf("phenology_statewide_%d.parquet", year_arg))
  write_parquet(pheno, pheno_parquet)
  message("  Wrote ", pheno_parquet, " (", nrow(pheno), " rows)")

  pheno_json_path <- file.path(out_dir, sprintf("phenology_statewide_%d.json", year_arg))
  pheno_list <- lapply(split(pheno, pheno$site_id), function(rows) {
    lapply(seq_len(nrow(rows)), function(i) {
      list(
        event_type = "phenology",
        year = rows$year[i],
        leafonday = rows$leafonday[i],
        leafoffday = rows$leafoffday[i]
      )
    })
  })
  write(toJSON(pheno_list, auto_unbox = TRUE, pretty = TRUE), pheno_json_path)
  message("  Wrote ", pheno_json_path)
}

#### PLANTING
if (run_planting) {
  message("[planting] Building events (C/N pools via LAI)")
  planting_date_str <- as.character(matched$mslsp_OGI)

  planting_rows <- vector("list", nrow(matched))
  for (i in seq_len(nrow(matched))) {
    row <- matched[i]
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
      planting_rows[[i]] <- data.table(
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
  planting_dt <- rbindlist(planting_rows, use.names = TRUE, fill = TRUE)
  setorder(planting_dt, site_id, year, season)
  planting_dt[, event_type := "planting"]
  setcolorder(planting_dt, c("event_type", setdiff(names(planting_dt), "event_type")))

  plant_parquet <- file.path(out_dir, sprintf("planting_statewide_%d.parquet", year_arg))
  write_parquet(planting_dt, plant_parquet)
  message("  Wrote ", plant_parquet, " (", nrow(planting_dt), " rows)")

  plant_json_path <- file.path(out_dir, sprintf("planting_statewide_%d.json", year_arg))
  plant_list <- lapply(split(planting_dt, planting_dt$site_id), function(rows) {
    lapply(seq_len(nrow(rows)), function(i) {
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
    })
  })
  write(toJSON(plant_list, auto_unbox = TRUE, pretty = TRUE), plant_json_path)
  message("  Wrote ", plant_json_path)
}

#### HARVEST (removal fractions for SIPNET / PEcAn; same LandIQ row as planting)
if (run_harvest) {
  message("[harvest] Building events (lookup-based removal fractions)")
  has_dest_col <- "destructive" %in% names(matched)

  harvest_rows <- vector("list", nrow(matched))
  for (i in seq_len(nrow(matched))) {
    row <- matched[i]
    code <- paste0(trimws(as.character(row$landiq_CLASS)), as.character(row$landiq_SUBCLASS))
    dest <- if (has_dest_col) isTRUE(as.logical(row$destructive[1])) else harvest_destructive_default
    h <- tryCatch(
      pool_env$initialize_harvest_from_lookup(
        ID = row$parcel_id,
        DATE = as.character(row$harvest_date_str)[1],
        code = code,
        PFT = row$landiq_PFT,
        lk = lk,
        destructive = dest
      ),
      error = function(e) NULL
    )
    if (!is.null(h) && nrow(h) > 0) {
      harvest_rows[[i]] <- data.table(
        site_id = row$parcel_id,
        year = row$year,
        season = row$season,
        date = as.character(row$harvest_date_str)[1],
        CLASS_SUBCLASS = code,
        PFT = row$landiq_PFT,
        frac_above_removed_0to1 = as.numeric(h$AGB_REMOVED[1]),
        frac_above_to_litter_0to1 = as.numeric(h$AGB_LITTER[1]),
        frac_below_removed_0to1 = as.numeric(h$BGB_REMOVED[1]),
        frac_below_to_litter_0to1 = as.numeric(h$BGB_LITTER[1])
      )
    }
    if (i %% 10000L == 0L) {
      message("  ", i, "/", nrow(matched), " done")
    }
  }
  harvest_dt <- rbindlist(harvest_rows, use.names = TRUE, fill = TRUE)
  setorder(harvest_dt, site_id, year, season)
  harvest_dt[, event_type := "harvest"]
  setcolorder(harvest_dt, c("event_type", setdiff(names(harvest_dt), "event_type")))

  harvest_parquet <- file.path(out_dir, sprintf("harvest_statewide_%d.parquet", year_arg))
  write_parquet(harvest_dt, harvest_parquet)
  message("  Wrote ", harvest_parquet, " (", nrow(harvest_dt), " rows)")

  harvest_json_path <- file.path(out_dir, sprintf("harvest_statewide_%d.json", year_arg))
  harvest_list <- lapply(split(harvest_dt, harvest_dt$site_id), function(rows) {
    lapply(seq_len(nrow(rows)), function(i) {
      list(
        event_type = rows$event_type[i],
        date = rows$date[i],
        year = rows$year[i],
        season = rows$season[i],
        crop = rows$CLASS_SUBCLASS[i],
        PFT = rows$PFT[i],
        frac_above_removed_0to1 = rows$frac_above_removed_0to1[i],
        frac_above_to_litter_0to1 = rows$frac_above_to_litter_0to1[i],
        frac_below_removed_0to1 = rows$frac_below_removed_0to1[i],
        frac_below_to_litter_0to1 = rows$frac_below_to_litter_0to1[i]
      )
    })
  })
  write(toJSON(harvest_list, auto_unbox = TRUE, pretty = TRUE), harvest_json_path)
  message("  Wrote ", harvest_json_path)
}

#### TILLAGE (buffered multi-year assigned + NDTI; parcel chunks)
if (run_tillage) {
  suppressPackageStartupMessages(library(dplyr))
  if (!file.exists(tillage_metrics_script)) {
    stop("Missing tillage_metrics.R: ", tillage_metrics_script)
  }
  source(tillage_metrics_script)

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
    mslsp_parts[[length(mslsp_parts) + 1L]] <- as.data.table(read_parquet(f))
  }
  if (length(mslsp_parts) == 0L) {
    stop("[tillage] No assigned parquet for load years")
  }

  mslsp_all <- rbindlist(mslsp_parts, use.names = TRUE, fill = TRUE)
  mslsp_all[, parcel_id := as.character(parcel_id)]
  mslsp_all <- mslsp_all[assigned_by == "matched"]
  mslsp_all <- mslsp_all[!is.na(mslsp_OGI) & !is.na(mslsp_OGMn)]
  mslsp_all[, OGI_date := as.Date(mslsp_OGI)]
  mslsp_all[, OGMn_date := as.Date(mslsp_OGMn)]
  mslsp_all <- mslsp_all[!is.na(OGI_date) & !is.na(OGMn_date)]

  phenology_full <- mslsp_all[, .(parcel_id, year, OGI_date, OGMn_date)]
  pft_y <- mslsp_all[, .(PFT = landiq_PFT[1L]), by = .(parcel_id, year)]

  message("[tillage] phenology rows ", nrow(phenology_full), " | parcels ", uniqueN(phenology_full$parcel_id))

  read_ndti_for_parcels <- function(parcel_ids, yrs, root) {
    pid_unique <- unique(as.character(parcel_ids))
    parts <- list()
    for (y in yrs) {
      ydir <- file.path(root, sprintf("year=%d", y))
      if (!dir.exists(ydir)) {
        next
      }
      # open_dataset(ydir) fails when year=* contains non-parquet (e.g. logs/*.log)
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
        parts[[length(parts) + 1L]] <- as.data.table(sub)
      }
    }
    if (length(parts) == 0L) {
      return(data.table())
    }
    rbindlist(parts, use.names = TRUE, fill = TRUE)
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
      results[[ic]] <- as.data.table(res)
    }
  }

  all_res <- rbindlist(results[!vapply(results, is.null, NA)], use.names = TRUE, fill = TRUE)
  if (nrow(all_res) == 0L) {
    stop("[tillage] No results (check NDTI overlap and errors above)")
  }

  all_res[, parcel_id := as.character(parcel_id)]
  all_res[, year := as.integer(year)]

  for (y in year_first:year_last) {
    yr_dt <- all_res[year == y]
    pq <- file.path(out_dir, sprintf("tillage_statewide_%d.parquet", y))
    js <- file.path(out_dir, sprintf("tillage_statewide_%d.json", y))

    if (nrow(yr_dt) == 0L) {
      message("[tillage] year ", y, ": no rows; writing empty outputs")
      write_parquet(
        data.table(event_type = character(), parcel_id = character(), year = integer()),
        pq
      )
      writeLines("{}", js)
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

    out_dt <- copy(yr_dt)
    out_dt[, site_id := parcel_id]
    out_dt[, event_type := "tillage"]
    date_cols <- names(out_dt)[vapply(out_dt, function(z) inherits(z, "Date"), NA)]
    for (cn in date_cols) {
      out_dt[, (cn) := as.character(get(cn))]
    }
    setcolorder(out_dt, c("event_type", setdiff(names(out_dt), "event_type")))
    write_parquet(out_dt, pq)
    message("  Wrote ", pq, " (", nrow(out_dt), " rows)")

    json_list <- lapply(split(out_dt, out_dt$site_id), function(rows) {
      lapply(seq_len(nrow(rows)), function(i) {
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
      })
    })
    write(toJSON(json_list, auto_unbox = TRUE, pretty = TRUE), js)
    message("  Wrote ", js)
  }
}

message("[make_events_statewide] Done for year=", year_arg)
