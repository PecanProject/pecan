
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
    physiology <- subset(read.csv(file.path(output_dir, "physiology-subdaily.txt"), header = T, sep = "\t"),
                         select = c("datetime", "species", "lai", "dC_co2_upt.kgCm.2.", "dC_maintenance_resp.kgCm.2.",
                                    "dC_transport_resp.kgCm.2.", "dC_growth_resp.kgCm.2.", "DW_below.kgDWm.2.", "DW_above.kgDWm.2."))
    
    
    soilchemistry <- subset(read.csv(file.path(output_dir, "soilchemistry-subdaily.txt"), header = T, sep ="\t"),
                            select = c("datetime", "sC_co2_hetero.kgCm.2."))
    
    # Soil moisture information
    watercycle <- subset(read.csv(file.path(output_dir, "watercycle-subdaily.txt"), header = T, sep ="\t"),
                         select = c("datetime", "soilwater_10cm...", "soilwater_30cm..."))
    
    
    # Harvest
    harvest <- subset(read.csv(file.path(output_dir, "report-harvest.txt"), header = T, sep ="\t"),
                      select = c("datetime", "dC_fru_export.kgCha.1.", "dC_fol_export.kgCha.1.", "dC_frt_export.kgCha.1.",
                                 "dC_lst_above_export.kgCha.1.", "dC_lst_below_export.kgCha.1.", "dC_dst_above_export.kgCha.1.",
                                 "dC_dst_below_export.kgCha.1.", "dC_straw_export.kgCha.1."))
    harvest$total <- rowSums(harvest[,-1])
    harvest <- harvest[,c("datetime", "total")]
    
    # Cut
    cut <- subset(read.csv(paste(output_dir, "report-cut.txt", sep = "/"), header = T, sep ="\t"),
                      select = c("datetime", "dC_fru_export.kgCha.1.", "dC_fol_export.kgCha.1.", "dC_dfol_export.kgCha.1.",
                                 "dC_lst_export.kgCha.1.", "dC_dst_export.kgCha.1.", "dC_frt_export.kgCha.1."))
    
    cut$total <- rowSums(cut[,-1])
    cut <- cut[,c("datetime", "total")]
    
  } else{
    PEcAn.logger::logger.severe("Subdaily output files not found, check the configurations for the LDNDC runs")
  }
  

  # This approach should be more reliable compared to previous since just choose one unique datetime
  # and the last one will be the "all", if there are several species on the field
  physiology <- physiology[!duplicated(physiology$datetime, fromLast = T),] 
  
  
  # Combine harvest and cut as one event
  harvest <- rbind(harvest, cut) %>% dplyr::group_by(.data$datetime) %>% dplyr::summarise(harvest_carbon_flux = sum(.data$total)/10000) %>%
    as.data.frame()
  
  # Temporary solution to get "no visible binding" note off from the variables: 'Date', 'Year' and 'Day'
  Date <- Year <- Day <- Step <- NULL
  
  ## Merge subdaily-files
  ldndc.raw.out <- merge(physiology, soilchemistry, by = 'datetime', all = TRUE)
  ldndc.raw.out <- merge(ldndc.raw.out, watercycle, by = 'datetime', all = TRUE)
  ldndc.raw.out <- merge(ldndc.raw.out, harvest, by = 'datetime', all = TRUE)
  
  ldndc.out <- ldndc.raw.out %>%
    dplyr:: mutate(Date = format(as.POSIXct(.data$datetime, format = "%Y-%m-%d")), .keep = "unused") %>%
    dplyr::slice(1:(dplyr::n()-1)) %>% # Removing one extra line in output
    dplyr::mutate(Year = lubridate::year(Date), Day = as.numeric(strftime(Date, format = "%j")),
           Step = rep(0:(length(which(Date %in% unique(Date)[1]))-1),len = length(Date))) %>%
    dplyr::select("Year", "Day", "Step", "lai", "dC_maintenance_resp.kgCm.2.", "dC_transport_resp.kgCm.2.",
                  "dC_growth_resp.kgCm.2.", "dC_co2_upt.kgCm.2.", "sC_co2_hetero.kgCm.2.",
                  "DW_below.kgDWm.2.", "DW_above.kgDWm.2.", "soilwater_10cm...",
                  "soilwater_30cm...", "harvest_carbon_flux")
  
  
  
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
    
    print(paste("---- Prosessing year: ", y))
    
    # Subset data for prosessing
    sub.ldndc.out <- subset(ldndc.out, Year == y)
    
    
    
    
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
    sub.ldndc.out <- subset(sub.ldndc.out, Day >= begin_date & Day <= end_d)

    
    # Create the tvals that are used in nc-files
    tvals <- sub.ldndc.out[["Day"]] + sub.ldndc.out[["Step"]] /out_day -1
    
    
    ## Outputs need to be an appropriate units, this can be done here
    output <- list()
    
    # LAI
    output[[1]] <- ifelse(!is.na(sub.ldndc.out$lai), sub.ldndc.out$lai, 0)
    
    # Photosynthesis rate - GPP
    GPP <- ifelse(!is.na(sub.ldndc.out$dC_co2_upt.kgCm.2.), sub.ldndc.out$dC_co2_upt.kgCm.2./timestep.s, 0)
    output[[2]] <- GPP
    
    # Autotrophic respiration
    Autotrophic <- ifelse(!is.na((sub.ldndc.out$dC_maintenance_resp.kgCm.2. + sub.ldndc.out$dC_transport_resp.kgCm.2. + sub.ldndc.out$dC_growth_resp.kgCm.2.)),
                          (sub.ldndc.out$dC_maintenance_resp.kgCm.2. + sub.ldndc.out$dC_transport_resp.kgCm.2. + sub.ldndc.out$dC_growth_resp.kgCm.2.)/timestep.s, 0)   
    output[[3]] <- Autotrophic
    
    # Heterotrophic respiration
    Heterotrophic <- sub.ldndc.out$sC_co2_hetero.kgCm.2./timestep.s
    output[[4]] <- Heterotrophic
    
    # Total respiration
    output[[5]] <- Autotrophic + Heterotrophic
    
    # NPP
    output[[6]] <- GPP - Autotrophic
    
    # NEE
    output[[7]] <- ifelse(!is.na(Autotrophic), Autotrophic, 0) + Heterotrophic - GPP
    
    # Soilmoisture at 10 cm
    output[[8]] <- c(t(data.frame(sub.ldndc.out$soilwater_10cm..., sub.ldndc.out$soilwater_30cm...)))
    
    # Aboveground biomass
    output[[9]] <- ifelse(!is.na(sub.ldndc.out$DW_above.kgDWm.2.), sub.ldndc.out$DW_above.kgDWm.2., 0)/timestep.s
    
    # Belowground biomass
    # Using constant 0.45 to calculate the C from dry matter
    output[[10]] <- ifelse(!is.na(sub.ldndc.out$DW_below.kgDWm.2.), sub.ldndc.out$DW_below.kgDWm.2., 0) * 0.45 / timestep.s
    
    harvest <- ifelse(!is.na(sub.ldndc.out$harvest_carbon_flux), sub.ldndc.out$harvest_carbon_flux, 0) * 0.45 / timestep.s
    output[[11]] <- harvest
    
    
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
    
    
    depth <- ncdf4::ncdim_def("depth", "m", vals = c(.10, .30))
    
    dims <- list(lon = lon, lat = lat, time = t)
    dims_added <- list(lon = lon, lat = lat, depth = depth, time = t)
    
    #dims_daily <- list(lon = lon, lat = lat, time = t_daily)
    time_interval <- ncdf4::ncdim_def(name = "hist_interval", 
                                      longname="history time interval endpoint dimensions",
                                      vals = 1:2, units="")
    
    
    
    ## Declare netCDF variables ##
    nc_var <- list()
    
    # Subdaily values
    nc_var[[1]] <- PEcAn.utils::to_ncvar("LAI", dims)
    nc_var[[2]] <- PEcAn.utils::to_ncvar("GPP", dims)
    nc_var[[3]] <- PEcAn.utils::to_ncvar("AutoResp", dims)
    nc_var[[4]] <- PEcAn.utils::to_ncvar("HeteroResp", dims)
    nc_var[[5]] <- PEcAn.utils::to_ncvar("TotalResp", dims)
    nc_var[[6]] <- PEcAn.utils::to_ncvar("NPP", dims)
    nc_var[[7]] <- PEcAn.utils::to_ncvar("NEE", dims)
    
    # Soilwater
    nc_var[[8]] <- PEcAn.utils::to_ncvar("SoilMoist", dims_added)
    
    # Biomass aboveground and belowground
    nc_var[[9]] <- ncdf4::ncvar_def("AGB", units = "kg C m-2", dim = dims, missval = -999,
                                    longname = "above ground biomass")
    nc_var[[10]] <- ncdf4::ncvar_def("below_ground_carbon_content", units = "kg C m-2", dim = dims, missval = -999,
                                     longname = "below ground biomass")
    
    nc_var[[length(nc_var)+1]] <- ncdf4::ncvar_def("harvest_carbon_flux", units = "kg m-2", dim = dims, missval = -999,
                                                   longname = "biomass of harvested organs")
    
    # Daily values
   # nc_var[[7]] <- PEcAn.utils::to_ncvar("LAI_Daily", dims_daily)
    
    
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
