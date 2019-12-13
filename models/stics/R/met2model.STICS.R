#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Converts a met CF file to a model specific met file. The input
##' files are calld <in.path>/<in.prefix>.YYYY.cf
##'
##' @name met2model.STICS
##' @title Write STICS met files
##' @param in.path path on disk where CF file lives
##' @param in.prefix prefix for each file
##' @param outfolder location where model specific output is written.
##' @param start_date start date of the simulation
##' @param end_date end date of the simulation
##' @param overwrite logical: replace output files if they already exist?
##' @return results 
##' @export
##' @author Istem Fer
##-------------------------------------------------------------------------------------------------#
met2model.STICS <- function(in.path, in.prefix, outfolder, start_date, end_date,
                            overwrite = FALSE, ...) {
  
  in.path    <- "/fs/data1/pecan.data/dbfiles/Fluxnet2015_CF_gapfill_site_1-5095"
  in.prefix  <- "FLX_FR-Gri_FLUXNET2015_SUBSET_HH_2004-2013_1-3"
  outfolder  <- "/fs/data1/pecan.data/dbfiles/Fluxnet2015_STICS_site_1-5095"
  start_date <- "2007/01/01"
  end_date   <- "2013/12/31"
  overwrite  <- FALSE
  
  PEcAn.logger::logger.info("START met2model.STICS")
  
  start_date  <- as.POSIXlt(start_date, tz = "UTC")
  end_date    <- as.POSIXlt(end_date, tz = "UTC")
  start_year  <- lubridate::year(start_date)
  end_year    <- lubridate::year(end_date)
  
  # starting with the easiest case, full years
  # STICS looks for different input files for each year
  out.files      <- paste(in.prefix, seq(start_year, end_year), "climate", sep = ".")
  out.files.full <- file.path(outfolder, out.files)
  
  results <- data.frame(file = out.files.full,
                        host = PEcAn.remote::fqdn(),
                        mimetype = "text/plain",
                        formatname = "climate",
                        startdate = start_date,
                        enddate = end_date,
                        dbfile.name = out.files,
                        stringsAsFactors = FALSE)
  PEcAn.logger::logger.info("internal results")
  PEcAn.logger::logger.info(results)
  
  
  ## check to see if the outfolder is defined, if not create directory for output
  if (!file.exists(outfolder)) {
    dir.create(outfolder)
  }
  
  ctr <- 1
  
  ## loop over files
  for (year in seq(start_year, end_year)) {
    
    if (file.exists(out.files.full[ctr]) && !overwrite) {
      PEcAn.logger::logger.debug("File '", out.files.full[ctr], "' already exists, skipping to next file.")
      next
    }
    
    simdays <- seq(lubridate::yday(start_date), PEcAn.utils::days_in_year(year))
    
    NDAYS      <- length(simdays)
    NWEATHER   <- as.integer(13)
    weather_df <- as.data.frame(matrix( -999.9, nrow = NDAYS, ncol = NWEATHER))
    
    # prepare data frame for STICS format, daily inputs, but doesn't have to be full year
    weather_df[ ,1] <- rep(gsub(".*_STICS_site_","", outfolder), simdays) # column 1: name of weather file
    weather_df[ ,2] <- rep(year, simdays) # column 2: year
    start_month <- ifelse(year == start_year, paste0(start_date), paste0(year, "/01/01"))
    end_month   <- ifelse(year == end_year,   paste0(end_date),   paste0(year, "/12/31"))

    weather_df[ ,3] <- lubridate::month(seq(lubridate::as_date(start_month), 
                                            lubridate::as_date(end_month), by = "day")) # column 3: month
    
    weather_df[ ,4] <- lubridate::mday(seq(lubridate::as_date(start_month), 
                          lubridate::as_date(end_month), by = "day")) # column 4: day in month
    weather_df[ ,5] <- simdays # column 5: Julian day
    
    
    # column 6: minimum temperature (°C)
    # column 7: maximum temperature (°C)
    # column 8: global radiation (MJ.m-2. j-1)
    # column 9: Penman PET (mm.j-1)
    # column 10: rainfall (mm.j-1)
    # column 11: wind (m.s-1)
    # column 12: vapour pressure (mbars)
    # column 13: CO2 content(ppm).
    
  } ## end-loop over files

  
  
} # met2model.STICS
