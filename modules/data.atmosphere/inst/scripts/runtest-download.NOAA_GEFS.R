#This program is to test the R script donload.NOAA.R during development.

test_no = 1
args = commandArgs(trailingOnly = TRUE)
if (length(args) > 0) {
  test_no = as.integer(args[1])
}

source("~/pecan/modules/data.atmosphere/R/download.NOAA_GEFS.R")

##' @param Output directory
##' @param lattitude of the site in decimal degrees
##' @param longitude of the site in decimal degrees
##' @param start date
##' @param end date
# Other parameters optional

if (test_no == 1) { #Default case - should work - normal 16 day forecast
  download.NOAA_GEFS("~/Working/results", lat.in= 46.2420, lon.in = -89.3476, "willow creek")
} else if (test_no == 2) { #Should be an Error - date out of bounds
  download.NOAA_GEFS("~/Working/results",lat.in= 46.2420, lon.in = -89.3476, "willow creek", Sys.time() - lubridate::days(12), Sys.time(), verbose = FALSE)
} else if (test_no == 3) { #Should work - normal 16 day forecast
  download.NOAA_GEFS("~/Working/results", lat.in= 46.2420, lon.in = -89.3476, "willow creek", Sys.time() - lubridate::days(4), verbose = FALSE)
} else if (test_no == 4) { #Should work - 1 day's worth of data
  download.NOAA_GEFS("~/Working/results", lat.in= 46.2420, lon.in = -89.3476, "willow creek", Sys.time() - lubridate::days(8), Sys.time() - lubridate::days(7),  verbose = FALSE)
} else if (test_no == 5) { #Should be an error - date out of bounds 
  download.NOAA_GEFS("~/Working/results", lat.in= 46.2420, lon.in = -89.3476, "willow creek", Sys.Date() + lubridate::days(1), verbose = FALSE)
} else if (test_no == 6) { #Should crash - timespan not large enough
  download.NOAA_GEFS("~/Working/results", lat.in= 46.2420, lon.in = -89.3476, "willow creek", Sys.time(), Sys.time(), verbose = FALSE)
} else if (test_no == 7) { #Should work, but have the timespan shrunk by one day.  Output should be identical to default case.
  download.NOAA_GEFS("~/Working/results", lat.in= 46.2420, lon.in = -89.3476, "willow creek", Sys.time(), Sys.time() + lubridate::days(17), verbose = FALSE)
}

  
  