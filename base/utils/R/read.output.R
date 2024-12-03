#' Read model output
#'
#' Reads the output of a single model run
#'
#' Generic function to convert model output from model-specific format to
#' a common PEcAn format. This function uses MsTMIP variables except that units of
#'  (kg m-2 d-1)  are converted to kg ha-1 y-1. Currently this function converts
#' Carbon fluxes: GPP, NPP, NEE, TotalResp, AutoResp, HeteroResp,
#' DOC_flux, Fire_flux, and Stem (Stem is specific to the BioCro model)
#' and Water fluxes: Evaporation (Evap), Transpiration (TVeg),
#' surface runoff (Qs), subsurface runoff (Qsb), and rainfall (Rainf).
#'
#' For more details, see the [MsTMIP
#' variables](http://nacp.ornl.gov/MsTMIP_variables.shtml)
#' documentation.
#'
#' @param runid the ID distinguishing the model run. Can be omitted
#'   if `ncfiles` is set.
#' @param outdir the directory that the model's output was sent to.
#'   Can be omitted if `ncfiles` is set.
#' @param variables Character vector of variables to be read from
#'   model output. Default = `"GPP"`. If `NULL`, try to read all
#'   variables in output file..
#' @param dataframe Logical: if TRUE, will return output in a
#'   `data.frame` format with a posix column. Useful for
#'   `PEcAn.benchmark::align.data` and plotting.
#' @param pft.name character string, name of the plant functional
#'   type (PFT) to read PFT-specific output. If `NULL` no
#'   PFT-specific output will be read even the variable has PFT as a
#'   dimension.
#' @param ncfiles Custom character vector of full paths to NetCDF
#'   files. If `NULL` (default), this list is constructed
#'   automatically by looking for `YYYY.nc` files in
#'   `file.path(outdir, runid)`.
#' @param verbose Logical. If `TRUE`, print status as every year and
#'   variable is read, as well as all NetCDF diagnostics (from
#'   `verbose` argument to, e.g., [ncdf4::nc_open()]) (default =
#'   `FALSE`).
#' @param print_summary Logical. If `TRUE` (default), calculate and
#'   print a summary of the means of each variable for each year.
#' @param start.year,end.year first and last year of output to read.
#'   Specify as a date-time (only the year portion is used) or as a
#'   four-digit number or string. If `NA`, reads all years found in
#'   `outdir`.
#' @return If `dataframe = FALSE`, a vector of output variables. If
#'   `dataframe = TRUE`, a `data.frame` of output variables with
#'   POSIXct timestamps added (`posix` column). The `posix` column
#'   is in seconds after January 1 of `start.year`, or 1970 if
#'   `start.year` is not provided.
#' @export
#' @author Michael Dietze, David LeBauer, Alexey Shiklomanov
read.output <- function(runid, outdir,
                        start.year = NA,
                        end.year = NA,
                        variables = "GPP",
                        dataframe = FALSE,
                        pft.name = NULL,
                        ncfiles = NULL,
                        verbose = FALSE,
                        print_summary = TRUE) {

  ## vars in units s-1 to be converted to y-1
  ## cflux = c('GPP', 'NPP', 'NEE', 'TotalResp', 'AutoResp', 'HeteroResp', 'DOC_flux', 'Fire_flux') # kgC m-2 s-1
  ## wflux = c('Evap', 'TVeg', 'Qs', 'Qsb', 'Rainf') # kgH20 m-2 d-1

  if ((missing(runid) || missing(outdir)) && is.null(ncfiles)) {
    PEcAn.logger::logger.severe(
      "`runid` or `outdir` is missing, and `ncfiles` is NULL.",
      "Either provide both `runid` and `outdir`, or explicitly specify all `ncfiles`."
    )
  }

  # Needed to allow runid and outdir to be missing
  if (!is.null(ncfiles)) {
    if (missing(runid)) runid <- NULL
    if (missing(outdir)) outdir <- NULL
  }

  # create list of *.nc years - look only for files formatted as
  # YYYY.nc, the default pecan output file name standard
  if (is.null(ncfiles)) {
    ncfiles_sub <- list.files(path = outdir, pattern = "^-?[[:digit:]]{4}\\.nc$", full.names = FALSE)
    ncfiles <- file.path(outdir, ncfiles_sub)
  } else {
    # Assume the NetCDF files follow the PEcAn standard format
    # (`YYYY.nc`). If they do not, pass `start.year` and `end.year`
    # explicitly.
    ncfiles_sub <- basename(ncfiles)
  }

  if (!is.na(start.year)) {
    if (lubridate::is.instant(start.year)) { # true if a Date, POSIXct, or POSIXlt
      start.year <- lubridate::year(start.year)
    } else if (is.character(start.year)) {
      start.year <- as.numeric(start.year)
    } else if (is.numeric(start.year)) {
      if (start.year %% 1 != 0) {
        PEcAn.logger::logger.severe(
          "Start year `", start.year, "` is numeric, but not an integer."
        )
      }
    } else {
      PEcAn.logger::logger.severe(
        "`start.year` must be of type numeric, character, Date, or POSIXt",
        "but given `", start.year, "` which is type `", typeof(start.year), "`."
      )
    }
  }

  if (!is.na(end.year)) {
    if (lubridate::is.instant(end.year)) {
      end.year <- lubridate::year(end.year)
    } else if (is.character(end.year)) {
      end.year <- as.numeric(end.year)
    } else if (is.numeric(end.year)) {
      if (end.year %% 1 != 0) {
        PEcAn.logger::logger.severe(
          "End year `", end.year, "` is numeric, but not an integer."
        )
      }
    } else {
      PEcAn.logger::logger.error(
        "`end.year` must be of type numeric, character, Date, or POSIXt",
        "but given `", end.year, "` which is type `", typeof(end.year), "`."
      )
    }
  }

  # Deduce file years from their names
  nc_years <- suppressWarnings(
    as.numeric(gsub("^(-?[[:digit:]]{4})\\.nc", "\\1", ncfiles_sub))
  )
  if (any(is.na(nc_years))) {
    PEcAn.logger::logger.debug(
      "Unable to deduce NetCDF file years from their names. ",
      "Setting `nc_years` to length 0 numeric vector."
    )
    nc_years <- numeric()
  }

  if (is.na(start.year)) {
    PEcAn.logger::logger.debug("Missing start year.")
    if (length(nc_years) != 0) start.year <- min(nc_years)
  }

  if (is.na(end.year)) {
    PEcAn.logger::logger.debug("Missing end year.")
    if (length(nc_years) != 0) end.year <- max(nc_years)
  }

  if (!is.na(start.year) && !is.na(end.year)) {
    # select only those *.nc years requested by user
    keep <- which(nc_years >= start.year & nc_years <= end.year)
    ncfiles <- ncfiles[keep]
  } else {
    PEcAn.logger::logger.info(
      "No start or end year provided; reading output for all years"
    )
  }

  origin_year <- start.year
  if (!is.finite(origin_year)) {
    PEcAn.logger::logger.warn(
      "Invalid (or missing) origin year `", origin_year, "`. ",
      "Setting origin year to 1970."
    )
    origin_year <- 1970
  }
  run_origin <- paste0(origin_year, "-01-01")

  # throw warning and return `NA` if no `*.nc` files selected/availible
  nofiles <- FALSE
  if (length(ncfiles) == 0) {
    PEcAn.logger::logger.warn(
      "read.output: no netCDF files of model output present",
      "for runid = ", runid,
      " in ", outdir,
      " for years ", start.year, ":", end.year, ".",
      "Returning NA."
    )
    if (length(nc_years) > 0) {
      PEcAn.logger::logger.info(
        "netCDF files for other years present: ",
        nc_years
      )
    }
    nofiles <- TRUE
  } else {
    PEcAn.logger::logger.info(
      "Reading the following files: ",
      normalizePath(ncfiles)
    )
  }

  result <- list()

  if (nofiles) {
    PEcAn.logger::logger.info("No files found. Returning all NA.")
    result <- lapply(variables, function(x) NA)
  } else {
    for (ncfile in ncfiles) {
      if (verbose) PEcAn.logger::logger.debug("Processing file: ", ncfile)
      nc <- ncdf4::nc_open(ncfile, verbose = verbose)
      if (is.null(variables)) {
        variables <- names(nc[["var"]])
        PEcAn.logger::logger.info(
          "Variables not specified. Reading output for all variables, which are as follows: ",
          paste(variables, collapse = ", ")
        )
      }

      if (dataframe) {
        seconds <- ud_convert(
          nc$dim$time$vals,
          nc$dim$time$units,
          paste("seconds since", run_origin)
        )
        result[["posix"]] <- abind::abind(result[["posix"]], seconds)
      }
      for (v in variables) {
        if (verbose) PEcAn.logger::logger.debug("Processing variable: ", v)
        if (!(v %in% c(names(nc$var), names(nc$dim)))) {
          PEcAn.logger::logger.warn(paste(v, "missing in", ncfile))
          next
        }
        newresult <- ncdf4::ncvar_get(nc, v, verbose = verbose)
        # begin per-pft read
        # check if the variable has 'pft' as a dimension
        if ("pft" %in% sapply(nc$var[[v]]$dim, `[[`, "name")) {
          # means there are PFT specific outputs we want
          # the variable *PFT* in standard netcdfs has *pft* dimension,
          # numbers as values, and full pft names as an attribute
          # parse pft names and match the requested
          pft.string <- ncdf4::ncatt_get(nc, "PFT", verbose = verbose)
          pft.ind <- strsplit(pft.string$long_name, ",")[[1]] == pft.name
          # dimensions can differ from model to model or run to run
          # there might be other cases that are not covered here
          dim.check <- length(dim(newresult))
          if (any(pft.ind)) { # means pft.name passed, we want to read pft-specific outputs
            if (dim.check == 1){
              newresult <- newresult[pft.ind]
            } else {
              newresult <- newresult[, pft.ind]
            }
          } else {
            # means this variable is available as per-pft, so written as such to standard ncdf files
            # but we still want to read as total
            if (dim.check == 1){
              newresult <- sum(newresult)
            } else {
              newresult <- apply(newresult, 1, sum)
            }
          }
        } # end of per-pft read

        # Dropping attempt to provide more sensible units because of graph unit errors,
        # issue #792
        # if (v %in% c(cflux, wflux)) {
        #   newresult <- ud_convert(newresult, 'kg m-2 s-1', 'kg ha-1 yr-1')
        # }
        result[[v]] <- abind::abind(result[[v]], newresult)
      }
      ncdf4::nc_close(nc)
    }

    if (print_summary) {
      result_means <- vapply(result, mean, numeric(1), na.rm = TRUE)
      result_medians <- vapply(result, stats::median, numeric(1), na.rm = TRUE)
      summary_matrix <- signif(cbind(Mean = result_means, Median = result_medians), 3)
      rownames(summary_matrix) <- names(result)
      PEcAn.logger::logger.info(
        "Result summary:\n",
        PEcAn.logger::print2string(summary_matrix),
        wrap = FALSE
      )
    }
  }

  if (!dataframe) return(result)

  # Check if there are variables that have multiple dimensions for
  # example soil moisture at multiple levels. Currently we don't have
  # a consensus how to convert these to dataframe format so they
  # should be omitted.

  for (var in names(result)) {
    c <- dim(result[[var]])[2]
    r <- dim(result[[var]])[1]
    if (!is.na(c) & r > 1) {
      PEcAn.logger::logger.warn("Variable", var, "has", r, "dimensions,
      it cannot be loaded and will be omitted.")
      result[[var]] <- NULL
    }
  }

  model <- as.data.frame(result) # put into a data.frame
  model[["posix"]] <- as.POSIXct(model[["posix"]], origin = run_origin, tz = "UTC")
  model[["year"]] <- lubridate::year(model[["posix"]])

  return(model)
} # read.output
