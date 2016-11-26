##' @name load_data
##' @title load_data
##' @export
##' @param data.path character
##' @param format list
##' @param start_year numeric
##' @param end_year numeric
##' @param site list
##' @author Betsy Cowdery
##' Generic function to convert input files containing observational data to 
##' a common PEcAn format. 
load_data <- function(data.path, format, start_year = NA, end_year = NA, site = NA, 
                      vars.used.index, time.row = NULL) {
  
  library(PEcAn.utils)
  library(PEcAn.benchmark)
  library(lubridate)
  library(udunits2)
  library(dplyr)
  
  # Determine the function that should be used to load the data
  mimetype <- sub("-", "_", format$mimetype)
  fcn1 <- paste0("load_", format$file_name)
  fcn2 <- paste0("load_", mimetype)
  if (exists(fcn1)) {
    fcn <- match.fun(fcn1)
  } else if (exists(fcn2)) {
    fcn <- match.fun(fcn2)
  } else if (!exists(fcn1) & !exists(fcn2)) { 
    #To Do: call to DAP to see if conversion to csv is possible
    #Brown Dog API call from BDFiddle
    #source("bd.r")
    #output_file <- browndog.convert("https://bd-api.ncsa.illinois.edu", data.path, "csv", "./")
    #data.path <- output_file
    #for test token: 3c351518-668d-4007-96a8-f5574f00171a
    token = "3c351518-668d-4007-96a8-f5574f00171a"
    data.path = "/fs/data3/jam2767/veg_test/H_2015_Adult_Field_Data.xlsx"
    output_path = "/fs/data3/jam2767/veg_test"
    convert_file("https://bd-api.ncsa.illinois.edu", "H_2015_Adult_Field_Data.xlsx", output_path, "csv", token)
    #had paste0(data.path, format$file_name) instead of filename from example "H_2015_Adult_Field_Data.xlsx"
    #not doing anything about mimetypes not convertable by BD right now
    fcn <- match.fun(fcn2)
  } else {
<<<<<<< HEAD:modules/benchmark/R/load.data.R
    logger.warn("Brown Dog is currently unable to perform this conversion")
=======
    PEcAn.utils::logger.warn("no load_data for current mimetype - converting using browndog")
>>>>>>> dbe5564971e57447860d4931629037ac64a03558:modules/benchmark/R/load_data.R
  }
  
  out <- fcn(data.path, format, site, format$vars$input_name[c(vars.used.index, time.row)])
  
  # Convert loaded data to the same standard variable names and units
  
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
                      vars_used$pecan_name[i], u2))
        out[col] <- misc.convert(x, u1, u2)
        colnames(out)[col] <- vars_used$pecan_name[i]
      } else {
        PEcAn.utils::logger.error("Units cannot be converted")
      }  # This error should probably be thrown much earlier, like in query.format.vars - will move it eventually
    }
  }
  
  if(!is.null(time.row)){  
    # Need a much more spohisticated approach to converting into time format. 
    y <- dplyr::select(out, one_of(format$vars$input_name[time.row]))
    
    if(!is.null(site$time_zone)){
      tz = site$time_zone
    }else{
      tz = "UTC"
      PEcAn.utils::logger.warn("No site timezone. Assuming input time zone is UTC. This may be incorrect.")
    }
    
    out$posix <- strptime(apply(y, 1, function(x) paste(x, collapse = " ")), 
                          format=paste(format$vars$storage_type[time.row], collapse = " "), tz = tz)
  }
  
  return(out)
} # load_data

##' Future things to think about
##'   - error estimates
##'   - QAQC
##'   - STEPPS -> cov
##'   - MCMC samples
##'   - 'data products' vs raw data
##'   - Is there a generic structure to ovbs?
##' 
