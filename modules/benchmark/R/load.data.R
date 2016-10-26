##' @name load.data
##' @title load.data
##' @export
##' @param data.path character
##' @param format list
##' @param start_year numeric
##' @param end_year numeric
##' @param site list
##' @author Betsy Cowdery
##' Generic function to convert input files containing observational data to 
##' a common PEcAn format. 
load.data <- function(data.path, format, start_year = NA, end_year = NA, site = NA, 
                      vars.used.index, time.row = NULL) {
  
  library(PEcAn.utils)
  library(PEcAn.benchmark)
  library(lubridate)
  library(udunits2)
  library(dplyr)
  
  # Determine the function that should be used to load the data
  mimetype <- sub("-", "_", format$mimetype)
  fcn1 <- paste0("load.", format$file_name)
  fcn2 <- paste0("load.", mimetype)
  if (exists(fcn1)) {
    fcn <- match.fun(fcn1)
  } else if (exists(fcn2)) {
    fcn <- match.fun(fcn2)
  } else {
    logger.warn("no load data for current mimetype - converting using browndog")
  }
  
  out <- fcn(data.path, format, site, format$vars$input_name[c(vars.used.index, time.row)])
  
  # Convert loaded data to the same standard varialbe names and units
  
  vars_used <- format$vars[vars.used.index, ]
  
  for (i in seq_len(nrow(vars_used))) {
    col <- names(out) == vars_used$input_name[i]
    if (vars_used$input_units[i] == vars_used$pecan_units[i]) {
      print("match")
      colnames(out)[col] <- vars_used$pecan_name[i]
    } else {
      x <- as.matrix(out[col])
      u1 <- vars_used$input_units[i]
      u2 <- vars_used$pecan_units[i]
      if (udunits2::ud.are.convertible(u1, u2)) {
        print(sprintf("convert %s %s to %s %s",
                      vars_used$input_name[i], vars_used$input_units[i], 
                      vars_used$pecan_name[i], vars_used$pecan_units[i]))
        out[col] <- udunits2::ud.convert(x, u1, u2)[, 1]
        colnames(out)[col] <- vars_used$pecan_name[i]
      } else if (misc.are.convertible(u1, u2)) {
        print(sprintf("convert %s %s to %s %s", 
                      vars_used$input_name[i], u1, 
                      vars_used$pecan_name[i], 2))
        out[col] <- misc.convert(x, u1, u2)
        colnames(out)[col] <- vars_used$pecan_name[i]
      } else {
        logger.error("Units cannot be converted")
      }  # This error should probably be thrown much earlier, like in query.format.vars - will move it eventually
    }
  }

  if(!is.null(time.row)){  
     # Need a much more spohisticated approach to converting into time format. 
     y <- dplyr::select(out, one_of(format$vars$input_name[time.row]))
     out$posix <- strptime(apply(y, 1, function(x) paste(x, collapse = " ")), format=paste(format$vars$storage_type[time.row], collapse = " "), tz = "UTC")
  }

  return(out)
} # load.data

##' Future things to think about
##'   - error estimates
##'   - QAQC
##'   - STEPPS -> cov
##'   - MCMC samples
##'   - 'data products' vs raw data
##'   - Is there a generic structure to ovbs?
##' 
