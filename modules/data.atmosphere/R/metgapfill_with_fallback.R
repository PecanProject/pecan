#' Gap-fill CF meteorological data using a fallback dataset
#'
#' Checks variable coverage in a base CF meteorological NetCDF file and,
#' when required, fills missing values using a fallback CF NetCDF file.
#' Missing values in the base file are replaced only where matching fallback
#' values exist. The input files are never modified in place.
#'
#' @param primary_cf character. Path to primary CF NetCDF file
#' @param vars character vector. CF variable names to gap-fill
#' @param fallback_cf character. Path to fallback CF NetCDF file
#' @param out_file character. Path to output CF NetCDF file
#' @param coverage_threshold numeric. Minimum acceptable coverage (0–1)
#' @param align_time logical. If TRUE, restricts both CF files to their
#' overlapping timestamps before filling. No interpolation, resampling,
#' or timestep conversion is performed.
#' @return character. Path to gap-filled CF NetCDF file
#'
#' @noRd

metgapfill_with_fallback <- function(
  primary_cf,
  vars,
  fallback_cf,
  out_file,
  coverage_threshold = 1.0,
  align_time = FALSE
) {

  # ---- basic validation (NO out_file side effects)
  stopifnot(
    file.exists(primary_cf),
    is.character(vars),
    file.exists(fallback_cf),
    is.character(out_file)
  )

  # ---- enforce test contract: out_file must NOT exist
  if (file.exists(out_file)) {
    file.remove(out_file)
  }

  # ---- check coverage FIRST (no side effects)
  coverage_info <- check_met_coverage_for_fallback(
    cf_file = primary_cf,
    threshold = coverage_threshold
  )

  fill_vars <- intersect(vars, coverage_info$fill_vars)

# ---- ensure fallback for requested variables that contain missing values
if (length(fill_vars) == 0) {
  nc_test <- ncdf4::nc_open(primary_cf)
  on.exit(ncdf4::nc_close(nc_test), add = TRUE)

  get_fill <- function(nc, var) {
    a <- ncdf4::ncatt_get(nc, var, "_FillValue")
    if (!is.null(a) && isTRUE(a$hasatt) && !is.na(a$value)) return(a$value)

    a2 <- ncdf4::ncatt_get(nc, var, "missing_value")
    if (!is.null(a2) && isTRUE(a2$hasatt) && !is.na(a2$value)) return(a2$value)

    return(NA)
  }

  for (v in vars) {
    if (!(v %in% names(nc_test$var))) next

    vals <- ncdf4::ncvar_get(nc_test, v)
    fill_val <- get_fill(nc_test, v)

    missing_mask <- is.na(vals) |
                    is.nan(vals) |
                    (!is.na(fill_val) & vals == fill_val)

    if (any(missing_mask)) {
      fill_vars <- c(fill_vars, v)
    }
  }
  fill_vars <- unique(fill_vars)
}
  
  # ---- NO fallback required → return primary, DO NOTHING ELSE
  if (length(fill_vars) == 0) {
    file.copy(primary_cf, out_file, overwrite = TRUE)
    return(out_file)
  }

  # ---- perform coalescing ONLY now
  coalesce_na_cf_met(
    primary_cf   = primary_cf,
    secondary_cf = fallback_cf,
    vars         = fill_vars,
    out_file     = out_file,
    align_time   = align_time
  )
}

# -------------------------------------------------------------------
# Deferred work (intentionally out of scope for this function)
# -------------------------------------------------------------------
# TODO(#3605): resolve fallback source selection (ERA5 vs ERA5-Land)
# TODO(#3605): prepare fallback CF NetCDF from raw data sources
# TODO(#3605): wire into AmeriFlux_met_ensemble() and met.process()
