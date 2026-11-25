#' Write three-hour site flux means from PEcAn ensemble runs
#'
#' @param settings PEcAn settings with `outdir` (containing runs_manifest.csv)
#'   and `modeloutdir` (containing run_id/YYYY.nc).
#' @param years Integer calendar years.
#' @param site_map Data frame pairing every Ensemble `site_id` with a unique
#'   positive integer Yang `index`. Row order sets the output site axis.
#' @param outdir Directory for year=YYYY.rds files. Existing years are protected.
#' @param forcing Optional named character vector of SIPNET climate files keyed
#'   by run_id. Used only to verify drifting interval starts.
#' @return Paths to annual RDS files containing site_ids, UTC interval starts
#'   (time), and site-by-time matrices nee (kg C m^-2 s^-1) and le (W m^-2).
#'   Each flux averages its finite ensemble members independently.
#' @export
write_site_flux_means <- function(settings, years, site_map, outdir, forcing = NULL) {
  if (!length(years) || !is.numeric(years) || any(!is.finite(years)) ||
      any(years != as.integer(years)) || anyDuplicated(years)) stop("Invalid years")
  if (!all(c("outdir", "modeloutdir") %in% names(settings))) stop("Missing workflow paths")
  if (!all(c("site_id", "index") %in% names(site_map)) || !nrow(site_map) ||
      anyNA(site_map$site_id) || any(!nzchar(site_map$site_id)) || anyDuplicated(site_map$site_id) ||
      !is.numeric(site_map$index) || any(!is.finite(site_map$index)) ||
      any(site_map$index < 1 | site_map$index != as.integer(site_map$index)) ||
      anyDuplicated(site_map$index)) stop("Invalid or duplicate site mapping")
  manifest <- utils::read.csv(file.path(settings$outdir, "runs_manifest.csv"), colClasses = "character")
  if (!all(c("type", "site_id", "ens_num", "run_id") %in% names(manifest))) stop("Incomplete manifest")
  runs <- manifest[which(manifest$type == "Ensemble"), c("site_id", "ens_num", "run_id")]
  if (!nrow(runs) || anyNA(runs) || any(!nzchar(as.matrix(runs)))) stop("Incomplete Ensemble records")
  if (anyDuplicated(runs$run_id) || anyDuplicated(runs[c("site_id", "ens_num")])) {
    stop("Duplicate Ensemble record: ", runs$run_id[duplicated(runs$run_id) |
           duplicated(runs[c("site_id", "ens_num")])][1])
  }
  mismatch <- c(setdiff(runs$site_id, site_map$site_id), setdiff(site_map$site_id, runs$site_id))
  if (length(mismatch)) stop("Unmapped or absent Ensemble site: ", paste(mismatch, collapse = ", "))
  inventory <- table(runs$site_id, runs$ens_num)
  if (any(inventory == 0)) {
    pair <- which(inventory == 0, arr.ind = TRUE)[1, ]
    stop("Missing Ensemble member ", colnames(inventory)[pair[2]], " for site ", rownames(inventory)[pair[1]])
  }
  if (!is.null(forcing) && (!is.character(forcing) || is.null(names(forcing)) ||
      anyDuplicated(names(forcing)) || !all(names(forcing) %in% runs$run_id))) stop("Invalid forcing run IDs")
  if (!dir.exists(outdir) && !dir.create(outdir, recursive = TRUE)) stop("Cannot create ", outdir)
  files <- file.path(outdir, sprintf("year=%d.rds", years))
  if (any(file.exists(files))) stop("Annual output already exists: ", files[file.exists(files)][1])
  for (i in seq_along(years)) {
    clock <- NULL
    for (j in seq_len(nrow(site_map))) {
      ids <- sort(runs$run_id[runs$site_id == site_map$site_id[j]])
      for (k in seq_along(ids)) {
        id <- ids[k]
        path <- file.path(settings$modeloutdir, id, sprintf("%d.nc", years[i]))
        flux <- read_site_flux(path, years[i], unname(forcing[id]))
        if (is.null(clock)) {
          clock <- flux$time
          nee <- le <- matrix(NA_real_, nrow(site_map), length(clock))
        }
        if (!identical(clock, flux$time)) stop("Inconsistent three-hour clock: ", path)
        if (k == 1L) sums <- counts <- matrix(0, length(clock), 2L)
        values <- as.matrix(flux[c("NEE", "Qle")])
        finite <- is.finite(values)
        sums[finite] <- sums[finite] + values[finite]
        counts <- counts + finite
      }
      avg <- sums / counts
      avg[counts == 0] <- NA_real_
      nee[j, ] <- avg[, 1]
      le[j, ] <- avg[, 2]
    }
    annual <- list(site_ids = as.integer(site_map$index), time = clock, nee = nee, le = le)
    partial <- tempfile(tmpdir = outdir)
    tryCatch({
      saveRDS(annual, partial)
      if (!file.link(partial, files[i])) stop("Cannot publish ", files[i])
    }, finally = unlink(partial))
  }
  invisible(files)
}

read_site_flux <- function(path, year, forcing) {
  if (!file.exists(path)) stop("Missing Ensemble output: ", path)
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  axis <- nc$dim$time
  if (is.null(axis) || (!is.null(axis$calendar) && !axis$calendar %in%
      c("standard", "gregorian", "proleptic_gregorian"))) stop("Unsupported time axis: ", path)
  units <- c(NEE = "kg C m-2 s-1", Qle = "W m-2")
  for (name in names(units)) {
    v <- nc$var[[name]]
    if (is.null(v) || !identical(v$units, units[[name]]) ||
        !any(vapply(v$dim, function(d) d$name == "time", logical(1))) ||
        any(vapply(v$dim, function(d) d$name != "time" && d$len != 1L, logical(1)))) {
      stop("Invalid dimensions or units for ", name, ": ", path)
    }
  }
  flux <- PEcAn.utils::read.output(ncfiles = path, variables = names(units),
                                    start.year = year, end.year = year, dataframe = TRUE)
  labels <- as.numeric(flux$posix)
  if (!length(labels) || length(labels) != axis$len || any(!is.finite(labels))) stop("Invalid time: ", path)
  starts <- labels
  bounds <- ncdf4::ncatt_get(nc, "time", "bounds")
  if (bounds$hasatt) {
    v <- nc$var[[bounds$value]]
    dims <- vapply(v$dim, function(d) d$name, character(1))
    if (length(dims) != 2L || !("time" %in% dims)) stop("Invalid time bounds: ", path)
    b <- ncdf4::ncvar_get(nc, bounds$value, collapse_degen = FALSE)
    if (dims[1] == "time") b <- t(b)
    if (nrow(b) != 2L) stop("Invalid time bounds: ", path)
    starts <- as.numeric(PEcAn.utils::cf2datetime(b[1, ], axis$units))
    ends <- as.numeric(PEcAn.utils::cf2datetime(b[2, ], axis$units))
    if (any(!is.finite(c(starts, ends))) || any(abs(ends - starts - 10800) > 0.1) ||
        any(labels < starts - 5 | labels > ends + 5)) stop("Invalid three-hour bounds: ", path)
  }
  slots <- round(starts / 10800) * 10800
  if (any(abs(slots - starts) > 0.1)) {
    if (length(forcing) != 1L || is.na(forcing) || !file.exists(forcing)) {
      stop("Drifting interval starts require run-specific forcing: ", path)
    }
    f <- utils::read.table(forcing, colClasses = c(rep("numeric", 4), rep("NULL", 8)))
    f <- f[f[[1]] == year, , drop = FALSE]
    day <- as.numeric(as.POSIXct(sprintf("%d-01-01", year), tz = "UTC")) + (f[[2]] - 1) * 86400
    slots <- day + floor(f[[3]] / 3) * 10800
    if (nrow(f) != length(labels) || anyNA(f) || any(f[[2]] != floor(f[[2]])) ||
        any(f[[3]] < 0 | f[[3]] >= 24 | abs(f[[4]] * 86400 - 10800) > 0.1) ||
        any(abs(day + f[[3]] * 3600 - labels) > 30 | abs(starts - labels) > 5) ||
        any(floor(labels / 10800) * 10800 != slots)) stop("Forcing clock mismatch: ", path)
  }
  flux$time <- as.POSIXct(slots, origin = "1970-01-01", tz = "UTC")
  if (any(diff(slots) != 10800) || any(format(flux$time, "%Y", tz = "UTC") != year)) {
    stop("Duplicate, missing, or out-of-year three-hour slot: ", path)
  }
  flux
}
