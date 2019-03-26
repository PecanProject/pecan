#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


#--------------------------------------------------------------------------------------------------#
##'
##' Convert SIPNET DOY to datetime
##' 
##' @param sipnet_tval vector of SIPNET DOY values
##' @param base_year base year to calculate datetime from DOY
##' @param base_month reference month for converting from DOY to datetime 
##' @param force_cf force output to follow CF convention. Default FALSE
##'
##' @export
##'
##' @author Alexey Shiklomanov, Shawn Serbin
##' 
sipnet2datetime <- function(sipnet_tval, base_year, base_month = 1,
                            force_cf = FALSE) {
  base_date <- ISOdatetime(base_year, base_month, 1,
                           0, 0, 0, "UTC")
  base_date_str <- strftime(base_date, "%F %T %z", tz = "UTC")
  if (force_cf) {
    is_cf <- TRUE
  } else {
    # HACK: Determine heuristically
    # Is CF if first time step is zero
    is_cf <- sipnet_tval[[1]] == 0
  }
  
  if (is_cf) {
    cfval <- sipnet_tval
  } else {
    cfval <- sipnet_tval - 1
  }
  
  PEcAn.utils::cf2datetime(cfval, paste("days since", base_date_str))
}
#--------------------------------------------------------------------------------------------------#


#--------------------------------------------------------------------------------------------------#
##' Convert SIPNET output to netCDF
##'
##' Converts all output contained in a folder to netCDF.
##' @name model2netcdf.SIPNET
##' @title Function to convert SIPNET model output to standard netCDF format
##' @param outdir Location of SIPNET model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @param revision model revision
##' @param overwrite Flag for overwriting nc files or not
##'
##' @export
##' @author Shawn Serbin, Michael Dietze
model2netcdf.SIPNET <- function(outdir, sitelat, sitelon, start_date, end_date, delete.raw, revision, 
                                overwrite = FALSE) {

  ### Read in model output in SIPNET format
  sipnet_out_file <- file.path(outdir, "sipnet.out")
  sipnet_output <- read.table(sipnet_out_file, header = T, skip = 1, sep = "")
  #sipnet_output_dims <- dim(sipnet_output)

  ### Determine number of years and output timestep
  #start.day <- sipnet_output$day[1]
  num_years <- length(unique(sipnet_output$year))
  simulation_years <- unique(sipnet_output$year)
  
  # quick consistency check
  year_seq <- seq(lubridate::year(start_date), lubridate::year(end_date))
  if (length(year_seq) != num_years) {
    PEcAn.logger::logger.severe("Date range specified in model2netcdf.SIPNET() function call and 
                                total number of years in SIPNET output are not equal")
  }

  # check that specified years and output years match
  if (!all(simulation_years %in% year_seq)) {
    PEcAn.logger::logger.severe("Years selected for model run and SIPNET output years do not match ")
  }
  
  # get number of model timesteps per day
  out_day <- sum(sipnet_output$year == simulation_years[1] & 
                   sipnet_output$day == unique(sipnet_output$day)[2], 
                 na.rm = TRUE) # switched to day 2 in case first day is partial
  timestep.s <- 86400 / out_day
  
  ### Loop over years in SIPNET output to create separate netCDF outputs
  for (y in simulation_years) {
    if (file.exists(file.path(outdir, paste(y, "nc", sep = "."))) & overwrite == FALSE) {
      next
    }
    print(paste("---- Processing year: ", y))  # turn on for debugging

    ## Subset data for processing
    sub.sipnet.output <- subset(sipnet_output, year == y)
    sub.sipnet.output.dims <- dim(sub.sipnet.output)
    dayfrac <- 1 / out_day
    step <- head(seq(0, 1, by = dayfrac), -1)   ## probably dont want to use
                                                ## hard-coded "step" because 
                                                ## leap years may not contain 
                                                ## all "steps", or
                                                ## if model run doesnt start 
                                                ## at 00:00:00
    
    # try to determine if DOY is CF compliant (i.e. 0 based index) or not (1 base index)
    pecan_start_doy <- PEcAn.utils::datetime2cf(start_date, paste0("days since ",lubridate::year(start_date),"-01-01"), 
                                          tz = "UTC")
    tvals <- sub.sipnet.output[["day"]] + sub.sipnet.output[["time"]] / 24
    if (sub.sipnet.output[["day"]][1]-pecan_start_doy==1) {
      sub_dates <- sipnet2datetime(tvals, y, force_cf = FALSE)
    } else {
      sub_dates <- sipnet2datetime(tvals, y, force_cf = TRUE)
    }
    sub_dates_cf <- PEcAn.utils::datetime2cf(sub_dates, paste0("days since ",paste0(y,"-01-01")))

    # create netCDF time.bounds variable
    bounds <- array(data=NA, dim=c(length(sub_dates_cf),2))
    bounds[,1] <- sub_dates_cf
    bounds[,2] <- bounds[,1]+dayfrac
    # create time bounds for each timestep in t, t+1; t+1, t+2... format
    bounds <- round(bounds,4) 
    
    ## Setup outputs for netCDF file in appropriate units
    output       <- list()
    output[[1]]  <- (sub.sipnet.output$gpp * 0.001) / timestep.s  # GPP in kgC/m2/s
    ## output[[2]] <- (sub.sipnet.output$npp*0.001) / timestep.s # NPP in kgC/m2/s. Internal SIPNET
    ## calculation
    output[[2]]  <- (sub.sipnet.output$gpp * 0.001) / timestep.s - ((sub.sipnet.output$rAboveground *
                                                                       0.001) / timestep.s + (sub.sipnet.output$rRoot * 0.001) / timestep.s)  # NPP in kgC/m2/s. Post SIPNET calculation
    output[[3]]  <- (sub.sipnet.output$rtot * 0.001) / timestep.s  # Total Respiration in kgC/m2/s
    output[[4]]  <- (sub.sipnet.output$rAboveground * 0.001) / timestep.s + (sub.sipnet.output$rRoot *
                                                                               0.001) / timestep.s  # Autotrophic Respiration in kgC/m2/s
    output[[5]]  <- ((sub.sipnet.output$rSoil - sub.sipnet.output$rRoot) * 0.001) / timestep.s  # Heterotrophic Respiration in kgC/m2/s
    output[[6]]  <- (sub.sipnet.output$rSoil * 0.001) / timestep.s  # Soil Respiration in kgC/m2/s
    output[[7]]  <- (sub.sipnet.output$nee * 0.001) / timestep.s  # NEE in kgC/m2/s
    # output[[7]] <- rep(-999,sipnet.output.dims[1]) # CarbPools
    output[[8]] <- (sub.sipnet.output$plantWoodC * 0.001)  # Above ground wood kgC/m2
    output[[9]] <- (sub.sipnet.output$plantLeafC * 0.001)  # Leaf C kgC/m2
    output[[10]] <- (sub.sipnet.output$plantWoodC * 0.001) + (sub.sipnet.output$plantLeafC * 0.001) + 
      (sub.sipnet.output$coarseRootC + sub.sipnet.output$fineRootC) * 0.001 # Total living C kgC/m2
    output[[11]] <- (sub.sipnet.output$soil * 0.001) + (sub.sipnet.output$litter * 0.001)  # Total soil C kgC/m2
    if (revision == "r136") {
      output[[12]] <- (sub.sipnet.output$evapotranspiration * 10 * PEcAn.data.atmosphere::get.lv()) / timestep.s  # Qle W/m2
    } else {
      ## *** NOTE : npp in the sipnet output file is actually evapotranspiration, this is due to a bug in sipnet.c : ***
      ## *** it says "npp" in the header (written by L774) but the values being written are trackers.evapotranspiration (L806) ***
      ## evapotranspiration in SIPNET is cm^3 water per cm^2 of area, to convert it to latent heat units W/m2 multiply with :
      ## 0.01 (cm2m) * 1000 (water density, kg m-3) * latent heat of vaporization (J kg-1)
      ## latent heat of vaporization is not constant and it varies slightly with temperature, get.lv() returns 2.5e6 J kg-1 by default
      output[[12]] <- (sub.sipnet.output$npp * 10 * PEcAn.data.atmosphere::get.lv()) / timestep.s  # Qle W/m2
    }
    output[[13]] <- (sub.sipnet.output$fluxestranspiration * 10) / timestep.s  # Transpiration kgW/m2/s
    output[[14]] <- (sub.sipnet.output$soilWater * 10)  # Soil moisture kgW/m2
    output[[15]] <- (sub.sipnet.output$soilWetnessFrac)  # Fractional soil wetness
    output[[16]] <- (sub.sipnet.output$snow * 10)  # SWE
    output[[17]] <- sub.sipnet.output$litter * 0.001  ## litter kgC/m2

    #calculate LAI for standard output
    param <- read.table(file.path(gsub(pattern = "/out/",
                                 replacement = "/run/", x = outdir),
                            "sipnet.param"), stringsAsFactors = FALSE)
    id <- which(param[, 1] == "leafCSpWt")
    leafC <- 0.48
    SLA <- 1000 * leafC / param[id, 2] #SLA, m2/kgC
    output[[18]] <- output[[9]] * SLA # LAI

    output[[19]] <- sub.sipnet.output$fineRootC   * 0.001  ## fine_root_carbon_content kgC/m2
    output[[20]] <- sub.sipnet.output$coarseRootC * 0.001  ## coarse_root_carbon_content kgC/m2
    output[[21]] <- (sub.sipnet.output$woodCreation * 0.001) / 86400 ## kgC/m2/s - this is daily in SIPNET
    output[[22]] <- (sub.sipnet.output$plantWoodC + sub.sipnet.output$plantLeafC) * 0.001 # Total aboveground biomass kgC/m2
    output[[23]] <- c(rbind(bounds[,1], bounds[,2]))
    
    # ******************** Declare netCDF variables ********************#
    t <- ncdf4::ncdim_def(name = "time",
                   longname = "time",
                   units = paste0("days since ", y, "-01-01 00:00:00"),
                   vals = round(sub_dates_cf,4),
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
    
    ## ***** Need to dynamically update the UTC offset here *****

    for (i in seq_along(output)) {
      if (length(output[[i]]) == 0)
        output[[i]] <- rep(-999, length(t$vals))
    }

    # ******************** Declare netCDF variables ********************#
    mstmipvar <- PEcAn.utils::mstmipvar
    nc_var <- list()
    nc_var[[1]]  <- PEcAn.utils::to_ncvar("GPP", dims)
    nc_var[[2]]  <- PEcAn.utils::to_ncvar("NPP", dims)
    nc_var[[3]]  <- PEcAn.utils::to_ncvar("TotalResp", dims)
    nc_var[[4]]  <- PEcAn.utils::to_ncvar("AutoResp", dims)
    nc_var[[5]]  <- PEcAn.utils::to_ncvar("HeteroResp", dims)
    nc_var[[6]]  <- ncdf4::ncvar_def("SoilResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999,
                          longname = "Soil Respiration") #need to figure out standard variable for this output
    nc_var[[7]]  <- PEcAn.utils::to_ncvar("NEE", dims)
    # nc_var[[7]] <- mstmipvar('CarbPools', lat, lon, t, NA)
    nc_var[[8]] <- PEcAn.utils::to_ncvar("AbvGrndWood", dims)
    nc_var[[9]] <- PEcAn.utils::to_ncvar("leaf_carbon_content", dims)
    nc_var[[10]] <- PEcAn.utils::to_ncvar("TotLivBiom", dims)
    nc_var[[11]] <- PEcAn.utils::to_ncvar("TotSoilCarb", dims)
    nc_var[[12]] <- PEcAn.utils::to_ncvar("Qle", dims)
    nc_var[[13]] <- PEcAn.utils::to_ncvar("Transp", dims)
    nc_var[[14]] <- PEcAn.utils::to_ncvar("SoilMoist", dims)
    nc_var[[15]] <- PEcAn.utils::to_ncvar("SoilMoistFrac", dims)
    nc_var[[16]] <- PEcAn.utils::to_ncvar("SWE", dims)
    nc_var[[17]] <- PEcAn.utils::to_ncvar("litter_carbon_content", dims)
    nc_var[[18]] <- PEcAn.utils::to_ncvar("LAI", dims)
    nc_var[[19]] <- PEcAn.utils::to_ncvar("fine_root_carbon_content", dims)
    nc_var[[20]] <- PEcAn.utils::to_ncvar("coarse_root_carbon_content", dims)
    nc_var[[21]] <- ncdf4::ncvar_def("GWBI", units = "kg C m-2", dim = list(lon, lat, t), missval = -999,
                                     longname = "Gross Woody Biomass Increment")
    nc_var[[22]] <- ncdf4::ncvar_def("AGB", units = "kg C m-2", dim = list(lon, lat, t), missval = -999,
                                     longname = "Total aboveground biomass")
    nc_var[[23]] <- ncdf4::ncvar_def(name="time_bounds", units='', 
                                    longname = "history time interval endpoints", dim=list(time_interval,time = t), 
                                    prec = "double")
    
    # ******************** Create netCDF and output variables ********************#
    ### Output netCDF data
    nc      <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")), nc_var)
    ncdf4::ncatt_put(nc, "time", "bounds", "time_bounds", prec=NA)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep = ".")), "w")
    for (i in seq_along(nc_var)) {
      ncdf4::ncvar_put(nc, nc_var[[i]], output[[i]])
      cat(paste(nc_var[[i]]$name, nc_var[[i]]$longname), file = varfile, sep = "\n")
    }
    close(varfile)
    ncdf4::nc_close(nc)
  }  ### End of year loop

  ## Delete raw output, if requested
  if (delete.raw) {
    file.remove(sipnet_out_file)
  }
} # model2netcdf.SIPNET
#--------------------------------------------------------------------------------------------------#
### EOF
