##' @name load_data
##' @title load_data
##' @export
##' @param data.path character
##' @param format list
##' @param start_year numeric
##' @param end_year numeric
##' @param site list
##' @author Betsy Cowdery, Istem Fer, Joshua Mantooth
##' Generic function to convert input files containing observational data to 
##' a common PEcAn format. 
load_data <- function(data.path, format, start_year = NA, end_year = NA, site = NA, 
                      vars.used.index=NULL, time.row = NULL) {

  ## load everything in format by default
  if(is.null(time.row)){
    time.row <- format$time.row 
  }
  if(is.null(vars.used.index)){
    vars.used.index <- setdiff(seq_along(format$vars$variable_id), format$time.row)
  }
  
  library(PEcAn.utils)
  library(PEcAn.benchmark)
  library(lubridate)
  library(udunits2)
  library(dplyr)
  
  # Determine the function that should be used to load the data
  mimetype <- gsub("-", "_", format$mimetype)
  fcn1 <- paste0("load_", format$file_name)
  fcn2 <- paste0("load_", mimetype)
  if (exists(fcn1)) {
    fcn <- match.fun(fcn1)
  } else if (exists(fcn2)) {
    fcn <- match.fun(fcn2)
  } else if (!exists(fcn1) & !exists(fcn2) & require(bd)) { 
    #To Do: call to DAP to see if conversion to csv is possible
    #Brown Dog API call through BDFiddle, requires username and password
    key   <- get_key("https://bd-api.ncsa.illinois.edu",username,password)
    token <- get_token("https://bd-api.ncsa.illinois.edu",key)
    #output_path = where are we putting converted file?
    converted.data.path <- convert_file(url = "https://bd-api.ncsa.illinois.edu", input_filename = data.path, 
                                        output = "csv", output_path = output_path, token = token)
    if (is.na(converted.data.path)){
      PEcAn.utils::logger.error("Converted file was not returned from Brown Dog")
    }
    #not doing anything about mimetypes not convertible by BD right now
    fcn <- match.fun("load_csv")
    data.path <- converted.data.path
  } else {
    PEcAn.utils::logger.warn("Brown Dog is currently unable to perform conversion from ",mimetype," to a PEcAn usable format")
  }
  
  out <- fcn(data.path, format, site, format$vars$input_name[c(vars.used.index, time.row)])
  
  # Convert loaded data to the same standard variable names and units
  
  vars_used <- format$vars[vars.used.index, ]
  
  # check wide format and transform to long
  if(any(duplicated(vars_used$bety_name))){
    w2l       <- wide2long(out, format, vars_used, time.row)
    out       <- w2l$mout
    format    <- w2l$format
    vars_used <- w2l$vars_used
    time.row  <- w2l$time.row
   }

  
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
