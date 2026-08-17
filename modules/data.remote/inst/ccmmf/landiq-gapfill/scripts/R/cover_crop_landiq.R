#!/usr/bin/env Rscript
# Flag cover crops on an existing LandIQ-style crop table.
#
# Definition (Violet / cover_crop.R):
#   COVER = TRUE when CLASS/SUBCLASS is a cover candidate in a non-dominant
#   season (not season 2), that parcel-year's season-2 CLASS is not G or P
#   (hay, grass, or pasture), and the crop differs from the previous cropped
#   season on the parcel (or this is the first cropped season). LandIQ lists
#   cover crops under G6 among other uses; P3 and P6 are also candidates.
#   Season 2 is never COVER = TRUE.
# Inactive seasons (no CLASS) are skipped by default.
#
# Product attachment: attach_cover_column() flags on seasons with a CLASS, then
# left-joins COVER back (inactive seasons stay COVER = NA).
#
# CLI (rewrites $LANDIQ_GAPFILLED/crops_all_years.parq):
#   Rscript "$LANDIQ_GAPFILL_ROOT/scripts/R/cover_crop_landiq.R"
#
# Library (sourced by load_landiq_gapfill):
#   crops <- attach_cover_column(crops)

suppressPackageStartupMessages(library(data.table))

#' Default CLASS/SUBCLASS pairs treated as cover-crop candidates.
default_cover_codes <- function() {
  data.table(
    CLASS = c("G", "P", "P"),
    SUBCLASS = as.character(c(6, 3, 6))
  )
}

#' Add a boolean COVER column to a LandIQ-style crop time series.
#'
#' Expects columns: parcel_id, year, season, CLASS, SUBCLASS.
#' All other columns are preserved. If drop_inactive = TRUE, rows with missing
#' CLASS are removed before flagging.
#'
#' @param crops data.frame / data.table of crop observations
#' @param cover_codes data.table with CLASS, SUBCLASS character columns
#' @param drop_inactive if TRUE, drop seasons with no CLASS before flagging
#' @return data.table with COVER added
flag_cover_crops <- function(
    crops,
    cover_codes = default_cover_codes(),
    drop_inactive = TRUE
) {
  dt <- as.data.table(copy(crops))

  required <- c("parcel_id", "year", "season", "CLASS", "SUBCLASS")
  missing <- setdiff(required, names(dt))
  if (length(missing) > 0) {
    stop("flag_cover_crops() missing required columns: ", paste(missing, collapse = ", "))
  }

  cover_codes <- as.data.table(cover_codes)[, .(
    CLASS = as.character(CLASS),
    SUBCLASS = as.character(SUBCLASS)
  )]

  dt[, `:=`(
    parcel_id = as.character(parcel_id),
    year = as.integer(year),
    season = as.integer(season)
  )]
  class_all <- trimws(as.character(dt$CLASS))
  is_dom <- dt$season == 2L
  dom <- unique(data.table(
    parcel_id = dt$parcel_id[is_dom],
    year = dt$year[is_dom],
    dom_class = class_all[is_dom]
  ), by = c("parcel_id", "year"))

  if (isTRUE(drop_inactive)) {
    dt <- dt[!is.na(class_all) & nzchar(class_all)]
  }

  dt <- merge(dt, dom, by = c("parcel_id", "year"), all.x = TRUE)
  setorder(dt, parcel_id, year, season)

  cover_key <- paste(cover_codes$CLASS, cover_codes$SUBCLASS, sep = "\r")
  cover_subclass <- paste(as.character(dt$CLASS), as.character(dt$SUBCLASS),
                          sep = "\r") %in% cover_key
  dom_chr <- trimws(as.character(dt$dom_class))
  dom_ok <- !is.na(dom_chr) & nzchar(dom_chr) & !(dom_chr %in% c("G", "P"))

  dt[, `:=`(
    prev_class = shift(CLASS),
    prev_sub = shift(SUBCLASS)
  ), by = parcel_id]
  prev_chr <- trimws(as.character(dt$prev_class))
  has_prev <- !is.na(prev_chr) & nzchar(prev_chr)
  alternates <- !has_prev |
    as.character(dt$CLASS) != as.character(dt$prev_class) |
    as.character(dt$SUBCLASS) != as.character(dt$prev_sub)

  dt[, COVER := fifelse(
    cover_subclass & season != 2L & dom_ok & alternates,
    TRUE, FALSE
  )]
  dt[, c("dom_class", "prev_class", "prev_sub") := NULL]
  dt
}

#' Attach COVER to a full LandIQ product table (including inactive season rows).
#'
#' Flags cover crops on seasons with a CLASS only, then left-joins COVER back.
#' Inactive / padded seasons keep COVER = NA.
#'
#' @param crops full LandIQ-style table
#' @param cover_codes passed to flag_cover_crops()
#' @return same rows as crops, with COVER (logical) added or replaced
attach_cover_column <- function(crops, cover_codes = default_cover_codes()) {
  dt <- as.data.table(copy(crops))
  if ("COVER" %in% names(dt)) {
    dt[, COVER := NULL]
  }

  flagged <- flag_cover_crops(dt, cover_codes = cover_codes, drop_inactive = TRUE)
  cover_map <- unique(flagged[, .(
    parcel_id = as.character(parcel_id),
    year = as.integer(year),
    season = as.integer(season),
    COVER = as.logical(COVER)
  )])

  dt[, `:=`(
    parcel_id = as.character(parcel_id),
    year = as.integer(year),
    season = as.integer(season)
  )]
  merge(dt, cover_map, by = c("parcel_id", "year", "season"), all.x = TRUE)
}

#' Attach COVER on the gap-filled product parquet and rewrite it.
attach_cover_to_gapfill_product <- function(
    path_parquet = file.path(landiq_product_root(), "crops_all_years.parq"),
    cover_codes = default_cover_codes()) {
  if (!file.exists(path_parquet)) {
    stop("Missing gap-filled product: ", path_parquet)
  }
  message("Attaching COVER on ", path_parquet)
  out <- arrow::read_parquet(path_parquet, as_data_frame = TRUE)
  out <- attach_cover_column(out, cover_codes = cover_codes)
  n_cover <- sum(out$COVER %in% TRUE, na.rm = TRUE)
  message("  COVER=TRUE rows: ", n_cover, " / ", nrow(out))
  arrow::write_parquet(out, path_parquet)
  message("Wrote ", nrow(out), " rows -> ", path_parquet)
  invisible(list(path_parquet = path_parquet, n_rows = nrow(out), n_cover = n_cover))
}

# Run as CLI only when this file is the Rscript entrypoint (not when sourced).
local({
  if (isTRUE(getOption("landiq.cover_crop_cli"))) {
    return(invisible(NULL))
  }
  ca <- commandArgs(trailingOnly = FALSE)
  file_arg <- sub("^--file=", "", grep("^--file=", ca, value = TRUE)[1L])
  ofile <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (!length(file_arg) || !nzchar(file_arg) || is.null(ofile)) {
    return(invisible(NULL))
  }
  if (!identical(
    normalizePath(file_arg, mustWork = FALSE),
    normalizePath(ofile, mustWork = FALSE)
  )) {
    return(invisible(NULL))
  }

  options(landiq.cover_crop_cli = TRUE)
  .libPaths(c(file.path(R.home(), "library"), .libPaths()))
  suppressPackageStartupMessages({
    library(tidyverse)
    library(arrow)
  })
  source(file.path(dirname(normalizePath(ofile, mustWork = FALSE)), "pkg_root.R"))
  load_landiq_gapfill()
  attach_cover_to_gapfill_product()
})
