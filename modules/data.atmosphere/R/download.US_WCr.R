##' @title download.US-WCr
##' 
##' @section General Description:
##' Obtains data from Ankur Desai's Willow Creek flux tower, and selects certain variables (NEE and LE) to return
##' Data is returned at the given timestep in the given range.
##' 
##' This data includes information on a number of flux variables.
##' 
##' The timestep parameter is measured in hours, but is then converted to half hours because the data's timestep
##' is every half hour.
##' 
##' @param start_date Start date/time data should be downloaded for
##' @param end_date End date/time data should be downloaded for
##' @param timestep How often to take data points from the file.  Must be a multiple of 0.5
##' @export
##' 
##' @author Luke Dramko
download.US_WCr <- function(start_date, end_date, timestep = 1) {
  timestep = 2 * timestep #data is actually every half hour
  
  if (timestep != as.integer(timestep)) {
    PEcAn.logger::logger.severe(paste0("Invalid timestep ", timestep/2, ". Timesteps must be at ",
                                       "least every half hour (timestep = 0.5)."))
  }
  
  start_date <- as.POSIXct(start_date, tz="UTC")
  end_date <- as.POSIXct(end_date, tz="UTC")
  
  nee_col = 9  # Column number of NEE
  le_col = 10  # Column number of LE
  
  # Data is found here
  # Original url: http://flux.aos.wisc.edu/data/cheas/wcreek/flux/prelim/wcreek2018_flux.txt
  base_url <- "http://co2.aos.wisc.edu/data/cheas/wcreek/flux/prelim/wcreek"
  
  flux = NULL;
  
  for (year in as.integer(format(start_date, "%Y")):as.integer(format(end_date, "%Y"))) {
    url <- paste0(base_url, year, "_flux.txt") #Build proper url
    PEcAn.logger::logger.info(paste0("Reading data for year ", year))
    print(url)
    influx <- tryCatch(utils::read.table(url, sep="", header=FALSE), error=function(e) {NULL}, warning=function(e) {NULL})
    if (is.null(influx)) { #Error encountered in data fetching.
      PEcAn.logger::logger.warn(paste0("Data not avaliable for year ", year, ". All values for ", year, " will be NA."))
      # Determine the number of days in the year
      rows_in_year <- PEcAn.utils::ud_convert(lubridate::as.duration(lubridate::interval(as.POSIXct(paste0(year, "-01-01")), as.POSIXct(paste0(year + 1, "-01-01")))), "s", "day")
      rows_in_year = rows_in_year * 48 # 48 measurements per day, one every half hour.
      influx <- matrix(rep(-999, rows_in_year * 13), nrow=rows_in_year, ncol = 13)
    }
    flux <- rbind(flux, influx)
  }
  PEcAn.logger::logger.info("Flux data has been read.")
  
  # Contains only the data needed in a data frame
  new.flux <- data.frame(DOY = flux[,3], 
           HRMIN = flux[,4],
           NEE = as.numeric(flux[,nee_col]),
           LE = as.numeric(flux[,le_col]))
  
  # Calculate minutes from start year to find the right row to pull data from.
  year_start <- as.POSIXct(format(start_date, "%Y-01-01 00:00:00"), tz="UTC")
  
  start_interval <- lubridate::interval(year_start, start_date)
  days <- lubridate::as.duration(start_interval)  # Actually returns a number of seconds
  days <- PEcAn.utils::ud_convert(as.integer(days), "s", "day") # Days, including fractional part, if any.
  hours <- floor(PEcAn.utils::ud_convert(days - floor(days), "day", "hr"))  # Extract the hour component, round to the previous hour.
  if (days - floor(days) >= 0.5) {  # Flux data is at half-hour precision
    hours <- hours + 0.5
  }
  days <- floor(days) # Extract the whole day component
  
  start_row <- as.integer(days * 48 + hours * 2)
  
  data_interval <- lubridate::interval(start_date, end_date)
  days <- lubridate::as.duration(data_interval) # a number of seconds
  days <- PEcAn.utils::ud_convert(as.integer(days), "s", "day")
  hours <- floor(PEcAn.utils::ud_convert(as.integer(days - floor(days)), "day", "hr")) # Round down to the nearest half hour
  if (days - floor(days) >= 0.5) {
    hours <- hours + 0.5
  }
  days <- floor(days)
  end_row <- start_row + as.integer(days * 48 + hours * 2)
  
  # Calculations are one time point behind the actual start time; corrects the off-by-one error
  start_row = start_row + 1;
  end_row = end_row + 1;
  
  # Vectors that will contain the output data
  out_nee = NULL
  out_le = NULL
  
  PEcAn.logger::logger.info("Starting at row (nonconverted) ")
  print(new.flux[start_row,]) #print gives a much more interpretable output than pasting in the logger call.
  
  for (d in seq(start_row, end_row, by=timestep)) {
    row = new.flux[d,]
    
    # NEE values
    val <- as.numeric(row$NEE)
    if (val == -999) { val <- NA } else {
        val <- PEcAn.utils::misc.convert(row$NEE, "umol C m-2 s-1", "kg C m-2 s-1")
    }
    out_nee <- c(out_nee, val)
    
    # LE values
    val <- as.numeric(row$LE)
    if (val == -999) { val <- NA }
    out_le <- c(out_le, val)
  }
  
  return(list(nee=out_nee[-1], qle=out_le[-1])) # Start time not included in the forecast
} # download.wcr.R

# This line is great for testing.
# download.US_WCr('2018-07-23 06:00', '2018-08-08 06:00', timestep=12)