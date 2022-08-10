
##-------------------------------------------------------------------------------------------------#
##' Convert LDNDC output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.LDNDC
##' @title Code to convert LDNDC's output into netCDF format
##'
##' @param outdir Location of model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @param delete.raw TRUE if raw model results will be deleted
##' @importFrom dplyr %>%
##' @importFrom rlang .data
##' @importFrom utils read.csv
##' @export
##'
##' @author Henri Kajasilta
model2netcdf.LDNDC <- function(outdir, sitelat, sitelon, start_date, end_date, delete.raw = FALSE) {


  # File path to Output directory wherein the raw model results are located 
  output_dir <- file.path(outdir, "Output")
  
  #### Something to check that data has same timesteps. Either take all of the necessary
  # files as a sub-daily or if not possible, then choose the daily option
  Subdailyfiles <- list.files(output_dir)[grep("*.-subdaily", list.files(output_dir))]
  
  # Test, if required files are all available subdaily
  ##---- Currently meant to work only with subdaily timesteps ----##
  if(all(c("physiology-subdaily.txt") %in% Subdailyfiles)){
    PEcAn.logger::logger.info("Files with sub-daily timesteps found: ", Subdailyfiles)
    
    # Physiology data: LAI, Photosynthesis rate
    physiology <- subset(read.csv(paste(output_dir, "physiology-subdaily.txt", sep = "/"), header = T, sep = "\t"),
                         select = c('datetime', 'lai', 'dC_co2_upt.kgCm.2.'))
  } else{
    PEcAn.logger::logger.info("Files with daily timesteps used")
    
    
    ## Ecosystem subset
    ecosystem <- subset(read.csv("ecosystem-daily.txt", header = T, sep = "\t"),
                        select = c('datetime', 'dC_NEE.kgCha.1.', 'C_total.kgCha.1.'))
  
  
    ## Physiology subset
    physiology <- subset(read.csv("physiology-daily.txt", header = T, sep = "\t"),
                         select = c('datetime', 'lai'))
  }
  
  
  
  # ldndc.out <- merge(ecosystem, physiology, by = "datetime", all = TRUE) %>%
  #   mutate(Date = format(as.POSIXlt(datetime, format = "%Y-%m-%d")), .keep = "unused") %>%
  #   mutate(Year = lubridate::year(Date), Day = strftime(Date, format = "%j"),
  #          Step = rep(0:(length(Date)/length(unique(Date))-1),length(unique(Date)))) %>%
  #   select(Year, Day, Step, dC_NEE.kgCha.1., C_total.kgCha.1., lai)
  
  # Temporary solution to get "no visible binding" note off from the variables: 'Date', 'Year' and 'Day'
  Date <- Year <- Day <- NULL
  
  ldndc.out <- physiology %>%
    dplyr:: mutate(Date = format(as.POSIXlt(.data$datetime, format = "%Y-%m-%d")), .keep = "unused") %>%
    dplyr::slice(1:(dplyr::n()-1)) %>% # Removing one extra observation
    dplyr::mutate(Year = lubridate::year(Date), Day = as.numeric(strftime(Date, format = "%j")),
           Step = rep(0:(length(which(Date %in% unique(Date)[1]))-1),len = length(Date))) %>%
    dplyr::select('Year', 'Day', 'Step', 'lai', 'dC_co2_upt.kgCm.2.')
  
  
  
  
  ## Check that the data match, based on the years we want
  simu_years <- unique(ldndc.out$Year)
  
  year_seq <- seq(lubridate::year(start_date), lubridate::year(end_date))
  
  
  ## Output is given sub-daily at this point --- Will see, if this will change later
  ## timesteps in a day
  out_day <- sum(ldndc.out$Year == simu_years[1] &
                 ldndc.out$Day == unique(ldndc.out$Day)[1],
                 na.rm = T)
  
  timestep.s <- 86400/out_day
  
  ## Loop over years in output to create separate netCDF outputs
  for(y in year_seq){
    # if file exist and overwrite is F, then move on to the next
    
    print(paste("---- Prosessing year: ", y)) # debugging
    
    # Subset data for prosessing
    sub.ldndnc.out <- subset(ldndc.out, Year == y)
    
    
    
    # Generate start/end dates for processing
    if (y == strftime(start_date, "%Y")) {
      begin_date <- lubridate::yday(start_date)
    } else {
      begin_date <- 1
    }
    
    if (y == strftime(end_date, "%Y")) {
      end_d <- lubridate::yday(end_date)
    } else {
      end_d <- PEcAn.utils::days_in_year(y)
    }
    
    ## Subset the years we are interested in
    sub.ldndnc.out <- subset(sub.ldndnc.out, Day >= begin_date & Day <= end_d)
    
    
    # Create the tvals that are used in nc-files
    tvals <- sub.ldndnc.out[["Day"]] + sub.ldndnc.out[["Step"]] /out_day -1
    
    
    
    
    ## Outputs need to be an appropriate units, this can be done here
    output <- list()
    # NEE value is on kg, so change it mg (*1 000 000), then change ha to m2 (/10 000) and then day to seconds (86400)
    #output[[1]] <- sub.ldndnc.out$dC_NEE.kgCha.1. * 100 / timestep.s
    
    # Kilogram of total soil carbon in a m2
    #output[[2]] <- sub.ldndnc.out$C_total.kgCha.1. / 10000
    
    # LAI
    output[[1]] <- sub.ldndnc.out$lai
    
    # Photosynthesis rate - GPP
    output[[2]] <- sub.ldndnc.out$dC_co2_upt.kgCm.2.
    
    #### Declare netCDF variables ####
    t <- ncdf4::ncdim_def(name = "time",
                          longname = "time",
                          units = paste0("days since ", y, "-01-01 00:00:00"), #00:00:00
                          vals = tvals,
                          calendar = "standard",
                          unlim = TRUE)
    
    
    
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), 
                            longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), 
                            longname = "station_longitude")
    dims <- list(lon = lon, lat = lat, time = t)
    time_interval <- ncdf4::ncdim_def(name = "hist_interval", 
                                      longname="history time interval endpoint dimensions",
                                      vals = 1:2, units="")
    
    
    
    ## Declare netCDF variables ##
    nc_var <- list()
    #nc_var[[1]] <- PEcAn.utils::to_ncvar("NEE", dims)
    #nc_var[[2]] <- PEcAn.utils::to_ncvar("TotSoilCarb", dims)
    nc_var[[1]] <- PEcAn.utils::to_ncvar("LAI", dims)
    nc_var[[2]] <- PEcAn.utils::to_ncvar("GPP", dims)
    
    
    
    ## Output netCDF data
    nc <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")), nc_var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep = ".")), "w")
    ncdf4::ncatt_put(nc, "time", "bounds", "time_bounds", prec = NA)
    
    
    
    for(i in seq_along(nc_var)){
      ncdf4::ncvar_put(nc, nc_var[[i]], output[[i]])
      cat(paste(nc_var[[i]]$name, nc_var[[i]]$longname), file = varfile, sep = "\n")
    }
    close(varfile)
    ncdf4::nc_close(nc)
    
  }
  # Delete the raw results
  if (delete.raw) {
    unlink(output_dir, recursive=TRUE)
  }
  
} # model2netcdf.LDNDC
