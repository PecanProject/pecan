#!/usr/bin/env Rscript

#' Split SIPNET inputs into multiple files based on start and end time
#'
#' Subset each SIPNET input file and write a new file containing values `>=
#' start.time` and `< end.time` (note: end time is *not* inclusive)
#'
#' NOTE that sipnet met files contain dates _and_ times, while sipnet event
#' files contain only dates. Comparing a datetime to a date will coerce the
#' date to midnight UTC.
#'
#' @section Efficiency:
#'
#' Splitting met files goes faster by far when all the inputs are known to have
#' a uniform timestep, fixed number of characters per line, and all splits are
#' at day boundaries. Setting `time_resolution = "days"` and
#' `allow_varying_timesteps = FALSE` opts in to a fast code path that finds the
#' wanted lines of the file by computing byte offsets and therefore avoids
#' needing to read every row of the input file.
#' For small subsets of large files this can be several times faster.
#'
#' If the input has variable line length but constant timesteps (as determined
#' by checking the first 10 lines; see the `n_head` argument of
#' `subset_met_lines()`), the fast path is taken but is not _as_ fast: it
#' must scan from the beginning to count line separators, but still skips the
#' tail of the file after `stop.time` is reached.
#'
#' @param start.time Start date or datetime for splitting
#' @param stop.time End date or datetime for splitting
#' @param inputs Named `inputs` list as provided by PEcAn `settings`. Must have
#' structure like:
#' `list(met = list(path = "path/to/sipnet.clim"), events = list(path = "path/to/events.in"), ...)`
#' @param time_resolution One of "row", "day". Should the split happen
#'  at the closest row to the end time or align with a time unit? See details
#' @param allow_varying_timesteps logical: Accept files where not all rows have
#'  the same duration? Disallowing can be substantially faster.
#'
#' @inheritParams split_sipnet_met
#' @author Alexey Shiklomanov
#'
#' @return Modified `inputs` list with all `path` entries replaced with new
#' paths (suitable for inserting back into `settings$run$inputs`).
#' @export
split_inputs.SIPNET <- function(
  start.time,
  stop.time,
  inputs,
  overwrite = FALSE,
  outpath = NULL,
  time_resolution = c("row", "day"),
  allow_varying_timesteps = TRUE
) {

  time_resolution <- match.arg(time_resolution)
  # TODO: might want to accept "year" here too
  if (time_resolution == "day") {
    start.time <- as.Date(start.time)
    stop.time <- as.Date(stop.time)
  }

  result <- inputs
  if ("met" %in% names(result)) {
    if (isFALSE(allow_varying_timesteps) && time_resolution != "row") {
      result[["met"]][["path"]] <- split_sipnet_met_by_line_count(
        start.time,
        stop.time,
        result$met$path,
        overwrite = overwrite,
        outpath = outpath
      )
    } else {
      result[["met"]][["path"]] <- split_sipnet_met(
        start.time,
        stop.time,
        result$met$path,
        overwrite = overwrite,
        outpath = outpath
      )
    }
  }

  if ("events" %in% names(result)) {
    result[["events"]][["path"]] <- split_sipnet_events(
      start.time,
      stop.time,
      result$events$path,
      overwrite = overwrite,
      outpath = outpath
    )
  }
  result
} # split_inputs.SIPNET



#' Split sipnet `events.in` files according to start and stop date
#'
#' @param eventfile Path to `events.in` file.
#' @inheritParams split_inputs.SIPNET
split_sipnet_events <- function(start.time, stop.time, eventfile, overwrite = FALSE, outpath = NULL) {
  # Read events.in
  prefix <- sub(".in", "", basename(eventfile), fixed = TRUE)
  outpath <- outpath %||% dirname(eventfile)
  dir.create(outpath, recursive = TRUE, showWarnings = FALSE)

  #Changing the name of the files, so it would contain the name of the hour as well.
  formatted_start <- strftime(start.time)
  formatted_stop <- strftime(stop.time)
  outfile <- file.path(outpath, paste0(prefix, ".", formatted_start, "-", formatted_stop, ".in"))
  names(outfile) <- paste(start.time, "-", stop.time)

  if (file.exists(outfile) && !overwrite) {
    PEcAn.logger::logger.warn(
      outfile,
      " already exists and overwrite is FALSE, so keeping existing file."
    )
    return(outfile)
  }

  # We can't use `read.table` or similar here because events file rows have
  # different numbers of columns depending on event type. Instead, we just
  # filter based on the event date, which is always the first two columns (year
  # and doy).
  events_in_raw <- readLines(eventfile)
  events_in_list <- strsplit(events_in_raw, "[[:space:]]+")
  years_in <- vapply(events_in_list, \(x) as.integer(x[[1]]), integer(1))
  doys_in <- vapply(events_in_list, \(x) as.integer(x[[2]]), integer(1))
  # Not using sipnet2datetime here because it returns times with time zones,
  # which could cause subtle timezone-related bugs
  dates_in <- as.Date(sprintf("%d-01-01", years_in)) + (doys_in - 1)
  idx_keep <- (dates_in >= start.time) & (dates_in < stop.time)
  if (length(idx_keep) == 0) {
    PEcAn.logger::logger.warn("No events to keep, so `events.in` will be empty")
  }
  events_out_str <- events_in_raw[idx_keep]
  writeLines(events_out_str, outfile)
  invisible(outfile)
} # split_sipnet_events



##' Extract subset of a sipnet clim file based on start and end time
##'
##' @author Mike Dietze, Ann Raiho, Alexey Shiklomanov
##'
##' @param start.time start date and time at which to start extraction
##' @param stop.time stop date and time at which to stop extraction
##' @param met path to sipnet clim file to be split
##' @param overwrite if `TRUE`, overwrite existing target file (Default `FALSE`)
##' @param outpath if specified, write output to a new directory. Default `NULL` writes back to the directory being read
##'
##' @return path to split up climate file
split_sipnet_met <- function(start.time, stop.time, met, overwrite = FALSE, outpath = NULL) {
  start.time <- coerce_to_datetime(start.time)
  stop.time <- coerce_to_datetime(stop.time)
  start.sip <- datetime2sipnet(start.time)
  stop.sip <- datetime2sipnet(stop.time)
  path <- dirname(met)
  prefix <- sub(".clim", "", basename(met), fixed = TRUE)
  if(is.null(outpath)){
    outpath <- path
  }
  if(!dir.exists(outpath)) dir.create(outpath, recursive = TRUE)


  file <- NA
  names(file) <- paste(start.time, "-", stop.time)

  #Changing the name of the files, so it would contain the name of the hour as well.
  formatted_start <- gsub(' ',"_", as.character(start.time))
  formatted_stop <- gsub(' ',"_", as.character(stop.time))
  file <- paste0(outpath, "/", prefix, ".", formatted_start, "-", formatted_stop, ".clim")

  if(file.exists(file) && !overwrite){
    PEcAn.logger::logger.warn(
      file,
      " already exists and overwrite is FALSE, so keeping existing file."
    )
    return(file)
  }

  input.dat <- utils::read.table(met, header = FALSE)


  if (ncol(input.dat) == 14) {
    # V1 format
    rows <- sipnet_time_range_idx(input.dat$V2, input.dat$V3, input.dat$V4,
                                  start.sip$year, start.sip$yday, start.sip$hour,
                                  stop.sip$year, stop.sip$yday, stop.sip$hour)
  } else if (ncol(input.dat) == 12) {
    rows <- sipnet_time_range_idx(input.dat$V1, input.dat$V2, input.dat$V3,
                                  start.sip$year, start.sip$yday, start.sip$hour,
                                  stop.sip$year, stop.sip$yday, stop.sip$hour)
  } else {
    PEcAn.logger::logger.error("Unknown clim format; can't split met files.")
    return(NA_character_)
  }

  dat <- input.dat[rows$start:rows$end,]

  ###### Write Met to file
  utils::write.table(dat, file, row.names = FALSE, col.names = FALSE)

  ###### Output input path to inputs
  #settings$run$inputs$met$path <- file
  return(file)
} # split_sipnet_met



coerce_to_datetime <- function(x) {
  if (inherits(x, "POSIXt")) {
    return(x)
  }
  xname <- deparse(substitute(x))
  if (!inherits(x, "Date")) {
    PEcAn.logger::logger.severe(
      "Invalid", xname, ":", x,
      "(class:", class(x), ")"
    )
  }
  PEcAn.logger::logger.debug(
    xname, "is a date, but this function expects a datetime.",
    "Coercing to datetime by setting to midnight UTC."
  )
  as.POSIXct(x, tz = "UTC")
} # coerce_to_datetime



split_sipnet_met_by_line_count <- function(
  start.time,
  stop.time,
  met,
  overwrite = FALSE,
  outpath = NULL
) {
  start.time <- as.Date(start.time)
  stop.time <- as.Date(stop.time)
  path <- dirname(met)
  prefix <- sub(".clim", "", basename(met), fixed = TRUE)
  if(is.null(outpath)){
    outpath <- path
  }
  if(!dir.exists(outpath)) dir.create(outpath, recursive = TRUE)


  file <- NA
  names(file) <- paste(start.time, "-", stop.time)
  file <- paste0(outpath, "/", prefix, ".", start.time, "-", stop.time, ".clim")

  if(file.exists(file) && !overwrite){
    PEcAn.logger::logger.warn(
      file,
      " already exists and overwrite is FALSE, so keeping existing file."
    )
    return(file)
  }

  subset_met_lines(clim_in = met, clim_out = file, start_day = start.time, stop_day = stop.time)

  return(file)
} # split_sipnet_met_by_line_count
