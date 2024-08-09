#' load data
#'
#' Generic function to convert input files containing observational data to
#' a common PEcAn format.
#'
#' @param data.path character
#' @param format list
#' @param start_year numeric
#' @param end_year numeric
#' @param site list
#' @param vars.used.index which variables to use? If NULL, these are taken from `format`
#' @param ... further arguments, currently ignored
#'
#' @author Betsy Cowdery, Istem Fer, Joshua Mantooth
#' @importFrom magrittr %>%
#' @export

load_data <- function(data.path, format, start_year = NA, end_year = NA, site = NA, 
                      vars.used.index=NULL, ...) {

  # If site = NA, check that site information is in the formats table
  if(all(is.na(site))){
    if(!is.null(format$site)){
      site <- list(id = format$site, lat = format$lat, lon = format$lon, 
                   time_zone = format$time_zone)
    }else{
      PEcAn.logger::logger.error("Input must have site information.")
    }
  }
  
  ## load everything in format by default
  time.row <- format$time.row 
  
  if(is.null(vars.used.index)){
    vars.used.index <- setdiff(seq_along(format$vars$variable_id), format$time.row)
  }
  
  # Determine the function that should be used to load the data
  mimetype <- gsub("-", "_", format$mimetype)
  fcn1 <- paste0("load_", format$file_name)
  fcn2 <- paste0("load_", mimetype)
  if (exists(fcn1)) {
    fcn <- match.fun(fcn1)
  } else if (exists(fcn2)) {
    fcn <- match.fun(fcn2)
  } else {
    PEcAn.logger::logger.warn("Brown Dog is currently unable to perform conversion from ",mimetype," to a PEcAn usable format")
  }
  
  vars =  format$vars$input_name[c(vars.used.index, time.row)]
  out <- fcn(data.path, format, site, vars)
  
  # Convert loaded data to the same standard variable names and units
  
  vars_used <- format$vars[vars.used.index, ]
  
  # check wide format and transform to long
  if(any(duplicated(vars_used$bety_name))){	
    w2l       <- format_wide2long(out, format, vars_used, time.row)		
    out       <- w2l$long_data		
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
      if (units::ud_are_convertible(u1, u2)) {
        print(sprintf("convert %s %s to %s %s",
                      vars_used$input_name[i], vars_used$input_units[i], 
                      vars_used$pecan_name[i], vars_used$pecan_units[i]))
        out[col] <- PEcAn.utils::ud_convert(as.numeric(x), u1, u2)
        colnames(out)[col] <- vars_used$pecan_name[i]
      } else if (PEcAn.utils::misc.are.convertible(u1, u2)) {
        print(sprintf("convert %s %s to %s %s", 
                      vars_used$input_name[i], u1, 
                      vars_used$pecan_name[i], u2))
        out[col] <- as.vector(PEcAn.utils::misc.convert(x, u1, u2)) # Betsy: Adding this because misc.convert returns vector with attributes original agrument x, which causes problems later
        colnames(out)[col] <- vars_used$pecan_name[i]
      } else {
        PEcAn.logger::logger.warn(paste("Units cannot be converted. Removing variable. please check the units of",vars_used$input_name[i]))
        out<-out[,!names(out) %in% c(vars_used$input_name[i])] 
        vars_used<-vars_used[!names(vars_used) %in% c(vars_used$input_name[i],vars_used$pecan_name[i]),]
      }
    }
  }
  
  if(!is.null(time.row)){  
    
    # load_data was not changing the name of the 'time' column
    col <- names(out) %in% format$vars$input_name[time.row]
    names(out)[col] <- format$vars$pecan_name[time.row]
    
    # Need a much more spohisticated approach to converting into time format. 
    y <- dplyr::select(out, tidyselect::one_of(format$vars$pecan_name[time.row]))
    
    if(!is.null(site$time_zone)){
      tz = site$time_zone
    }else{
      tz = "UTC"
      PEcAn.logger::logger.warn("No site timezone. Assuming input time zone is UTC. This may be incorrect.")
    }
    
    out$posix <- strptime(apply(y, 1, function(x) paste(x, collapse = " ")), 
                          format=paste(format$vars$storage_type[time.row], collapse = " "),
                          tz = tz) %>% as.POSIXct()
    }
  
  # Subset by start year and end year when loading data
  # This was part of the arguments but never implemented
  if(!is.na(start_year)){
    out$year <- lubridate::year(out$posix)
    out <- out %>% filter(.data$year >= as.numeric(start_year))
    print("subsetting by start year")
  }
  
  if(!is.na(end_year)){
    out$year <- lubridate::year(out$posix)
    out <- out %>% filter(.data$year <= as.numeric(end_year))
    print("subsetting by end year")
  }
  
  return(out)
} # load_data
