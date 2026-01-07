#' Extract monthly weather from CF file for input to RothC
#'
#' Input files need to be named `<in.path>/<in.prefix>.YYYY.nc`
#'
#' Output files are named `<outfolder>/<in.prefix>.YY-mm.YY-mm.dat`
#' with one line per month and columns for temperature, rainfall,
#' and evaporation.
#'
#' Note that the created file contains only weather data and not any of the
#' soil or management data needed for RothC's single combined input file.
#' See `write.config.RothC()` for assembly into a model-ready RothC_input.dat`.
#'
#' @param in.path path on disk where CF files live
#' @param in.prefix prefix for each file
#' @param outfolder location where model specific output is written.
#' @param start_date,end_date When to start and end output.
#'  Specify as exact dates, but output will be padded to whole months.
#' @param overwrite logical: replace output files if they already exist?
#' @return data frame summarizing file metadata
#' @export
#' @author Chris Black
met2model.RothC <- function(in.path,
                            in.prefix,
                            outfolder,
                            start_date,
                            end_date,
                            overwrite = FALSE) {

  PEcAn.logger::logger.info("START met2model.RothC")

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)
  start_year <- strftime(start_date, "%Y")
  end_year <- strftime(end_date, "%Y")
  year_regex <- paste(start_year:end_year, collapse = "|")

  if (grepl("\\.nc$", in.prefix)) {
    # Assume it's the full filename rather than a prefix
    # NB also means we assume it contains the whole requested date range
    name_pattern <- in.prefix
  } else {
    name_pattern <- paste0(in.prefix, "\\.(", year_regex, ")\\.nc$")
  }

  nc_files <- list.files(in.path, pattern = name_pattern, full.names = TRUE)

  if (length(nc_files) == 0) {
    PEcAn.logger::logger.severe(
      "No files found matching ", in.prefix,
      "for years", start_year, ":", end_year,
      "; cannot process data."
    )
  }

  # TODO complain (fail?) here if some but not all years found

  out_filename <- paste(
    in.prefix,
    strftime(start_date, "%Y-%m"),
    strftime(end_date, "%Y-%m"),
    "dat",
    sep = "."
  )
  out_path <- file.path(outfolder, out_filename)
  results <- data.frame(file = out_path,
                        host = Sys.getenv(
                          "FQDN",
                          unset = Sys.info()[["nodename"]]
                        ),
                        mimetype = "text/tab-separated-values",
                        formatname = "RothC.dat",
                        startdate = start_date,
                        enddate = end_date,
                        dbfile.name = out_filename,
                        stringsAsFactors = FALSE)
  PEcAn.logger::logger.info("internal results")
  PEcAn.logger::logger.info(results)

  if (file.exists(out_path) && !overwrite) {
    PEcAn.logger::logger.debug(
      "File '", out_path, "' already exists, skipping to next file."
    )
    return(invisible(results))
  }

  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }

  met <-  nc_files |>
    lapply(
      read_nc,
      varnames = c("air_temperature", "precipitation_flux", "specific_humidity")
    ) |>
    do.call(what = "rbind")

  # TODO probably need more care with partial months here:
  # we check if data extends to start/end, but not whether that includes enough
  # days to treat as a whole month.
  # e.g. if start_date = YYYY-01-31 and data starts YYYY-01-30 ->
  # current code will aggregate those two days as if they were all of January.
  # Consider failing if >n days missing in any output month?
  first_month <- lubridate::floor_date(start_date, unit = "month")
  last_month <- lubridate::ceiling_date(end_date, unit = "month")
  met <- met[(met$timestamp >=  first_month) & (met$timestamp < last_month), ]
  if (as.Date(min(met$timestamp)) > start_date
      || as.Date(max(met$timestamp)) < end_date) {
    PEcAn.logger::logger.severe(
      "input (",
      paste(range(met$timestamp), collapse = " to "),
      ") does not cover requested time window (",
      start_date, "to", end_date, ")"
    )
  }

  timestep <- met$timestamp |>
    diff(units = "secs") |>
    mean() |>
    as.numeric()

  met$year <- lubridate::year(met$timestamp)
  met$month <- lubridate::month(met$timestamp)
  met$Tmp_C <- met$air_temperature |>
    PEcAn.utils::ud_convert("K", "degC")
  met$Rain_mm <- met$precipitation_flux * timestep # kg/m2/sec -> mm total
  met$Evap_mm <- 0# TODO... how to convert Qair to pan evaporation?

  met_monthly <- merge(
    stats::aggregate(met, Tmp_C ~ year + month, mean),
    stats::aggregate(met, cbind(Rain_mm, Evap_mm) ~ year + month, sum),
    sort = FALSE # would treat months as strings; sort as numbers below instead
  )
  met_monthly <- met_monthly[order(met_monthly$year, met_monthly$month), ]

  utils::write.table(
    # as.data.frame to write integer columns as ints not floats
    x = format(as.data.frame(met_monthly), digits = 4),
    file = out_path,
    quote = FALSE,
    sep = "\t",
    row.names = FALSE,
    col.names = TRUE
  )

  results
}





# slurp named vars from one PEcAn nc into a dataframe with timestamp
#
# TODO could read other dimensions if present too, but consider if worth it --
# maybe this function is better for files where only the time dimension varies
#
# if vars = NULL, read all of them
read_nc <- function(ncfile, varnames = NULL) {

  nc <- ncdf4::nc_open(ncfile)
  on.exit(ncdf4::nc_close(nc), add = TRUE)

  timestamps <- PEcAn.utils::cf2datetime(
    nc$dim$time$vals,
    nc$dim$time$units
  )

  if (is.null(varnames)) {
    varnames <- names(nc$var)
  }

  var_values <- lapply(
    varnames,
    ncdf4::ncvar_get,
    nc = nc
  )

  # todo handle this case (multi-loc files?)
  stopifnot(all(sapply(var_values, length) == length(timestamps)))

  var_values |>
    stats::setNames(varnames) |>
    as.data.frame() |>
    transform(timestamp = timestamps)
}
