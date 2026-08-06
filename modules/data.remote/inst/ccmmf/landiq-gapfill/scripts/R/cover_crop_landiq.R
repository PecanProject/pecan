# Flag cover crops on an existing LandIQ-style crop table.
#
# Definition (Violet / cover_crop.R):
#   COVER = TRUE when CLASS/SUBCLASS is a cover candidate AND the row
#   alternates from the previous season on the same parcel (class or subclass).
# First observation per parcel cannot alternate -> COVER = FALSE.
# If subclass_source is present, absent seasons are dropped by default.
#
# Product attachment (padded LandIQ table): use attach_cover_column(), which
# flags on non-absent rows then left-joins COVER back (missing -> FALSE).
# Wired into landiq-gapfill build_landiq_product() and
# scripts/landiq/add_cover_column_to_product.R for one-shot patches.
#
# Usage:
#   source(file.path(Sys.getenv("CCMMF_CODE"), "landiq-gapfill/scripts/R/cover_crop_landiq.R"))
#   crops <- flag_cover_crops(crops)
#   crops <- attach_cover_column(crops)  # full product including absent seasons

suppressPackageStartupMessages(library(data.table))

#' Default CLASS/SUBCLASS pairs treated as cover-crop candidates.
default_cover_codes <- function() {
  data.table(
    CLASS = c("F", "F", "F", "F", "G", "G", "P", "P", "P", "P"),
    SUBCLASS = as.character(c(2, 11, 12, 16, 2, 6, 1, 3, 4, 6))
  )
}

#' Add a boolean COVER column to a LandIQ-style crop time series.
#'
#' Expects columns: parcel_id, year, season, CLASS, SUBCLASS.
#' All other columns are preserved. If subclass_source exists and
#' drop_absent = TRUE, rows with subclass_source == "absent" are removed.
#'
#' @param crops data.frame / data.table of crop observations
#' @param cover_codes data.table with CLASS, SUBCLASS character columns
#' @param drop_absent if TRUE and subclass_source exists, drop absent seasons
#' @return data.table with COVER added
flag_cover_crops <- function(
    crops,
    cover_codes = default_cover_codes(),
    drop_absent = TRUE
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

  if ("subclass_source" %in% names(dt) && isTRUE(drop_absent)) {
    dt <- dt[is.na(subclass_source) | as.character(subclass_source) != "absent"]
  }

  # Typed copies for matching / lag (leave original columns unchanged)
  class_chr <- as.character(dt$CLASS)
  subclass_chr <- as.character(dt$SUBCLASS)
  parcel_chr <- as.character(dt$parcel_id)

  setorderv(dt, c("parcel_id", "year", "season"))
  # Recompute after reorder
  class_chr <- as.character(dt$CLASS)
  subclass_chr <- as.character(dt$SUBCLASS)
  parcel_chr <- as.character(dt$parcel_id)

  cover_key <- paste(cover_codes$CLASS, cover_codes$SUBCLASS, sep = "\r")
  cover_subclass <- paste(class_chr, subclass_chr, sep = "\r") %in% cover_key

  prev_parcel <- shift(parcel_chr, 1L)
  prev_class <- shift(class_chr, 1L)
  prev_subclass <- shift(subclass_chr, 1L)
  same_parcel <- !is.na(prev_parcel) & parcel_chr == prev_parcel
  alternates <- same_parcel & (class_chr != prev_class | subclass_chr != prev_subclass)

  dt[, COVER := fifelse(cover_subclass & alternates %in% TRUE, TRUE, FALSE)]
  dt
}

#' Attach COVER to a full LandIQ product table (including padded absent seasons).
#'
#' Flags cover crops on non-absent rows only (Violet alternation rules), then
#' left-joins COVER back so padded seasons are preserved. Missing -> FALSE.
#'
#' @param crops full LandIQ-style table (may include subclass_source == "absent")
#' @param cover_codes passed to flag_cover_crops()
#' @return same rows as crops, with COVER (logical) added or replaced
attach_cover_column <- function(crops, cover_codes = default_cover_codes()) {
  dt <- as.data.table(copy(crops))
  if ("COVER" %in% names(dt)) {
    dt[, COVER := NULL]
  }

  flagged <- flag_cover_crops(dt, cover_codes = cover_codes, drop_absent = TRUE)
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
  dt <- merge(dt, cover_map, by = c("parcel_id", "year", "season"), all.x = TRUE)
  dt[is.na(COVER), COVER := FALSE]
  dt
}
