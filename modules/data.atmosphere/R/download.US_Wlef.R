##' @title download.US_Wlef
##' 
##' @section General Description:
##' Obtains data from Ankur Desai's WLEF/ Parks Fall flux tower, and selects certain variables (NEE and LE) to return
##' Data is returned at the given timestep in the given range.
##' 
##' This data includes information on a number of flux variables.
##' 
##' 
##' @param start_date Start date/time data should be downloaded for
##' @param end_date End date/time data should be downloaded for
##' @param timestep How often to take data points from the file.  Must be integer
##' @export
##' 
##' @author Luke Dramko and K Zarada
download.US_Wlef <- function(start_date, end_date, timestep = 1) {

  if (timestep != as.integer(timestep)) {
    PEcAn.logger::logger.severe(paste0("Invalid timestep ",timestep, ". Timesteps must be integer"))
  }
  
  start_date <- as.POSIXct(start_date, tz="UTC")
  end_date <- as.POSIXct(end_date, tz="UTC")
  
  nee_col = 7  # Column number of NEE
  le_col = 8 # Column number of LE
  
  # Data is found here
  # Original url: http://flux.aos.wisc.edu/data/cheas/wcreek/flux/prelim/wcreek2018_flux.txt
  base_url <- "http://flux.aos.wisc.edu/data/cheas/wlef/flux/prelim/"
  
  flux = NULL;
  
  for (year in as.integer(format(start_date, "%Y")):as.integer(format(end_date, "%Y"))) {
    url <- paste0(base_url, year,"/flux_", year, ".txt") #Build proper url
    PEcAn.logger::logger.info(paste0("Reading data for year ", year))
    print(url)
    influx <- tryCatch(utils::read.table(url, header = T, sep = ""), error=function(e) {NULL}, warning=function(e) {NULL})
    if (is.null(influx)) { #Error encountered in data fetching.
      PEcAn.logger::logger.warn(paste0("Data not avaliable for year ", year, ". All values for ", year, " will be NA."))
      # Determine the number of days in the year
      rows_in_year <- PEcAn.utils::ud_convert(lubridate::as.duration(lubridate::interval(as.POSIXct(paste0(year, "-01-01")), as.POSIXct(paste0(year + 1, "-01-01")))), "s", "day")
      rows_in_year = rows_in_year * 24 # 48 measurements per day, one every half hour.
      influx <- matrix(rep(-999, rows_in_year * 13), nrow=rows_in_year, ncol = 13)
    }
    flux <- rbind(flux, influx)
  }
  PEcAn.logger::logger.info("Flux data has been read.")
  
  # Contains only the data needed in a data frame
  new.flux <- data.frame(DOY = flux[,5], 
                         HR = flux[,4],
                         NEE = as.numeric(flux[,nee_col]),
                         LE = as.numeric(flux[,le_col]))
  
  # Calculate minutes from start year to find the right row to pull data from.
  year_start <- as.POSIXct(format(start_date, "%Y-01-01 00:00:00"), tz="UTC")
  
  start_interval <- lubridate::interval(year_start, start_date)
  days <- lubridate::as.duration(start_interval)  # Actually returns a number of seconds
  days <- PEcAn.utils::ud_convert(as.integer(days), "s", "day") # Days, including fractional part, if any.
  hours <- floor(PEcAn.utils::ud_convert(days - floor(days), "day", "hr"))  # Extract the hour component, round to the previous hour.
  days <- floor(days) # Extract the whole day component
  
  start_row <- as.integer(days * 24 + hours)
  
  data_interval <- lubridate::interval(start_date, end_date)
  days <- lubridate::as.duration(data_interval) # a number of seconds
  days <- PEcAn.utils::ud_convert(as.integer(days), "s", "day")
  hours <- floor(PEcAn.utils::ud_convert(as.integer(days - floor(days)), "day", "hr")) # Round down to the nearest half hour
  days <- floor(days)
  end_row <- start_row + as.integer(days * 24 + hours)
  
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
} # download.US_Syv.R

# This line is great for testing.
#download.US_Wlef('2018-07-23 06:00', '2018-08-08 06:00', timestep=12)
