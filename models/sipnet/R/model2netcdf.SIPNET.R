#' Merge multiple NetCDF files into one
#' 
#' @param files \code{character}. List of filepaths, which should lead to NetCDF files.
#' @param outfile \code{character}. Output filename of the merged data.
#' @return A NetCDF file containing all of the merged data.
#' @examples
#' \dontrun{
#' files <- list.files(paste0(system.file(package="processNC"), "/extdata"), 
#'                     pattern="tas.*\\.nc", full.names=TRUE)
#' temp <- tempfile(fileext=".nc")
#' mergeNC(files=files, outfile=temp)
#' terra::rast(temp) 
#' }
#' @export mergeNC
#' @name mergeNC
#' @source https://github.com/RS-eco/processNC/blob/main/R/mergeNC.R
mergeNC <- function(
    ##title<< Aggregate data in netCDF files
  files ##<< character vector: names of the files to merge
  , outfile ##<< character: path to save the results files to. 
)
  ##description<<
  ## This function aggregates time periods in netCDF files. Basically it is just a
  ## wrapper around the respective cdo function.
{
  ##test input
  #if (system("cdo -V")==0)
  #  stop('cdo not found. Please install it.')
  
  ## supply cdo command
  cdoCmd <- paste('cdo -cat', paste(files, collapse=" "), outfile, sep=' ')
  
  ##run command
  system(cdoCmd)
  cat(paste('Created file ', outfile, '.\n', sep = ''))
  
  ## character string: name of the file created. 
  invisible(outfile)
}

#--------------------------------------------------------------------------------------------------#
##' Convert SIPNET output to netCDF
##'
##' Converts all output contained in a folder to netCDF.
##'
##' @param outdir Location of SIPNET model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @param revision model revision
##' @param overwrite Flag for overwriting nc files or not
##' @param conflict Flag for dealing with conflicted nc files, if T we then will merge those, if F we will jump to the next.
##' @param prefix prefix to read the output files
##' @param delete.raw logical: remove sipnet.out files after converting?
##'
##' @export
##' @author Shawn Serbin, Michael Dietze
model2netcdf.SIPNET <- function(outdir, sitelat, sitelon, start_date, end_date, delete.raw = FALSE, revision, prefix = "sipnet.out",
                                overwrite = FALSE, conflict = FALSE) {
  ### Read in model output in SIPNET format
  sipnet_out_file <- file.path(outdir, prefix)
  # SIPNET v1 had a "Notes" comment line before the header; v2 removed it.
  # if the first line starts with "year", there is no Notes line.
  first_line <- readLines(sipnet_out_file, n = 1)
  skip_n <- if (grepl("^year", first_line)) 0 else 1
  sipnet_output <- utils::read.table(sipnet_out_file, header = T, skip = skip_n, sep = "")
  #sipnet_output_dims <- dim(sipnet_output)
  
  ### Determine number of years and output timestep
  #start.day <- sipnet_output$day[1]
  num_years <- length(unique(sipnet_output$year))
  simulation_years <- unique(sipnet_output$year)
  
  # get all years that we want data from
  year_seq <- seq(lubridate::year(start_date), lubridate::year(end_date))
  
  # check that specified years and output years match
  if (!all(year_seq %in% simulation_years)) {
    PEcAn.logger::logger.severe("Years selected for model run and SIPNET output years do not match ")
  }
  
  # get number of model timesteps per day
  # outday is the number of time steps in a day - for example 6 hours would have out_day of 4
  
  out_day <- sum(
    sipnet_output$year == simulation_years[1] &
      sipnet_output$day == unique(sipnet_output$day)[1],
    na.rm = TRUE
  ) # switched to day 2 in case first day is partial
  
  
  timestep.s <- 86400 / out_day
  
  
  ### Loop over years in SIPNET output to create separate netCDF outputs
  for (y in year_seq) {
    #initialize the conflicted as FALSE
    conflicted <- FALSE
    conflict <- TRUE    #conflict is set to TRUE to enable the rename of yearly nc file for merging SDA results with sub-annual data
    #if we have conflicts on this file.
    if (file.exists(file.path(outdir, paste(y, "nc", sep = "."))) & overwrite == FALSE & conflict == FALSE) {
      next
    }else if(file.exists(file.path(outdir, paste(y, "nc", sep = "."))) & conflict){
      conflicted <- TRUE
      file.rename(file.path(outdir, paste(y, "nc", sep = ".")), file.path(outdir, "previous.nc"))
    }
    print(paste("---- Processing year: ", y))  # turn on for debugging
    
    ## Subset data for processing
    sub.sipnet.output <- subset(sipnet_output, sipnet_output$year == y)
    
    sub_dates <- sipnet2datetime(
      y,
      sub.sipnet.output[["day"]],
      sub.sipnet.output[["time"]]
    )
    
    sub_dates_cf <- PEcAn.utils::datetime2cf(
      sub_dates,
      paste0("days since ", y, "-01-01"),
      tz = "UTC"
    )
    
    sub.sipnet.output.dims <- dim(sub.sipnet.output)
    dayfrac <- 1 / out_day
    
    # create netCDF time.bounds variable
    bounds <- array(data=NA, dim=c(length(sub_dates_cf),2))
    bounds[,1] <- sub_dates_cf
    bounds[,2] <- bounds[,1]+dayfrac
    # create time bounds for each timestep in t, t+1; t+1, t+2... format
    bounds <- round(bounds,4) 
    
    ## Setup outputs for netCDF file in appropriate units
    output <- list(
      "GPP" = PEcAn.utils::ud_convert(sub.sipnet.output$gpp, "g/m2", "kg/m2") / timestep.s,
      "NPP" = PEcAn.utils::ud_convert(sub.sipnet.output$gpp - (sub.sipnet.output$rAboveground + sub.sipnet.output$rRoot), "g/m2", "kg/m2") / timestep.s,
      "TotalResp" = PEcAn.utils::ud_convert(sub.sipnet.output$rtot, "g/m2", "kg/m2") / timestep.s,
      "AutoResp" = (PEcAn.utils::ud_convert(sub.sipnet.output$rAboveground + sub.sipnet.output$rRoot, "g/m2", "kg/m2")) / timestep.s,
      "HeteroResp" = PEcAn.utils::ud_convert(sub.sipnet.output$rSoil - sub.sipnet.output$rRoot, "g/m2", "kg/m2") / timestep.s,
      "SoilResp" = PEcAn.utils::ud_convert(sub.sipnet.output$rSoil, "g/m2", "kg/m2") / timestep.s,
      "NEE" = PEcAn.utils::ud_convert(sub.sipnet.output$nee, "g/m2", "kg/m2") / timestep.s,
      "AbvGrndWood" = PEcAn.utils::ud_convert(sub.sipnet.output$plantWoodC, "g/m2", "kg/m2"),
      "leaf_carbon_content" = PEcAn.utils::ud_convert(sub.sipnet.output$plantLeafC, "g/m2", "kg/m2"),
      "TotLivBiom" = (PEcAn.utils::ud_convert(sub.sipnet.output$plantWoodC + sub.sipnet.output$plantLeafC +
                                              sub.sipnet.output$coarseRootC + sub.sipnet.output$fineRootC, "g/m2", "kg/m2")),
      "TotSoilCarb" = PEcAn.utils::ud_convert(sub.sipnet.output$soil + sub.sipnet.output$litter, "g/m2", "kg/m2")
    )

    # Water variables:
    # Liquid water units are cm in Sipnet; in PEcAn they're kg water m-2
    #  (which is equivalent to mm, but ud_convert doesn't know that)
    # Evapotranspiration in SIPNET is cm^3 water per cm^2 of area, to convert it to latent heat units W/m2 multiply with :
    #  0.01 (cm2m) * 1000 (water density, kg m-3) * latent heat of vaporization (J kg-1)
    # Latent heat of vaporization is not constant and it varies slightly with temperature, get.lv() returns 2.5e6 J kg-1 by default
    output[["Qle"]] <- (PEcAn.utils::ud_convert(sub.sipnet.output$evapotranspiration, "cm", "mm") * PEcAn.data.atmosphere::get.lv()) / timestep.s  # Qle W/m2
    # Note that Sipnet reports transpiration, and no other variables, in cm/day not cm/timestep.
    output[["Transp"]] <- PEcAn.utils::ud_convert(sub.sipnet.output$fluxestranspiration, "cm/day", "mm/sec") # Transpiration
    output[["SoilMoist"]] <- PEcAn.utils::ud_convert(sub.sipnet.output$soilWater, "cm", "mm")  # Soil moisture kgW/m2
    output[["SoilMoistFrac"]] <- PEcAn.utils::ud_convert(sub.sipnet.output$soilWetnessFrac, "cm", "mm")  # Fractional soil wetness
    output[["SWE"]] <- PEcAn.utils::ud_convert(sub.sipnet.output$snow, "cm", "mm")  # Snow Water Equivalent
    output[["litter_carbon_content"]] <- PEcAn.utils::ud_convert(sub.sipnet.output$litter, "g/m2", "kg/m2")
    # litterWater was removed in SIPNET v2; only extract if present
    if ("litterWater" %in% names(sub.sipnet.output)) {
      output[["litter_mass_content_of_water"]] <- PEcAn.utils::ud_convert(sub.sipnet.output$litterWater, "cm", "mm")
    }

    #calculate LAI for standard output
    # LAI = plantLeafC / leafCSpWt
    # both operands are in carbon units (gC/m2 and gC/m2_leaf),
    # so no carbon fraction conversion (e.g. cFracLeaf) is needed.
    param <- utils::read.table(file.path(gsub(pattern = "/out/",
                                              replacement = "/run/", x = outdir),
                                         "sipnet.param"), stringsAsFactors = FALSE)
    leafCSpWt <- param[param[, 1] == "leafCSpWt", 2]
    SLA <- 1000 / leafCSpWt  # m2 leaf / kg C
    output[["LAI"]] <- output[["leaf_carbon_content"]] * SLA
    output[["fine_root_carbon_content"]] <- PEcAn.utils::ud_convert(sub.sipnet.output$fineRootC, "g/m2", "kg/m2")
    output[["coarse_root_carbon_content"]] <- PEcAn.utils::ud_convert(sub.sipnet.output$coarseRootC, "g/m2", "kg/m2")
    if ("woodCreation" %in% names(sub.sipnet.output)) {
      output[["GWBI"]] <- PEcAn.utils::ud_convert(sub.sipnet.output$woodCreation, "g/m2/day", "kg/m2/s")
    }
    output[["AGB"]] <- PEcAn.utils::ud_convert(sub.sipnet.output$plantWoodC + sub.sipnet.output$plantLeafC, "g/m2", "kg/m2")
    # columns only present in sipnet >= v2 with N and methane turned on
    if ("minN" %in% names(sub.sipnet.output)) {
      output[["mineral_N"]] <- sub.sipnet.output$minN * 0.001  # gN/m2 -> kgN/m2
    }
    if ("soilOrgN" %in% names(sub.sipnet.output)) {
      output[["soil_organic_N"]] <- sub.sipnet.output$soilOrgN * 0.001  # kgN/m2
    }
    if ("litterN" %in% names(sub.sipnet.output)) {
      output[["litter_N"]] <- sub.sipnet.output$litterN * 0.001  # kgN/m2
    }
    if ("n2o" %in% names(sub.sipnet.output)) {
      output[["N2O_flux"]] <- PEcAn.utils::ud_convert(sub.sipnet.output$n2o, "g/m2", "kg/m2") / timestep.s
      # convert g N m-2 per timestep -> kg N m-2 s-1
    }
    if ("nLeaching" %in% names(sub.sipnet.output)) {
      output[["N_leaching"]] <- (sub.sipnet.output$nLeaching * 0.001) / timestep.s  # kgN/m2/s
    }
    if ("nFixation" %in% names(sub.sipnet.output)) {
      output[["N_fixation"]] <- (sub.sipnet.output$nFixation * 0.001) / timestep.s  # kgN/m2/s
    }
    if ("nUptake" %in% names(sub.sipnet.output)) {
      output[["N_uptake"]] <- (sub.sipnet.output$nUptake * 0.001) / timestep.s  # kgN/m2/s
    }
    if ("ch4" %in% names(sub.sipnet.output)) {
      output[["CH4_flux"]] <- PEcAn.utils::ud_convert(sub.sipnet.output$ch4, "g/m2", "kg/m2") / timestep.s
      # convert g C m-2 per timestep -> kg C m-2 s-1
    }
    output[["time_bounds"]] <- c(rbind(bounds[,1], bounds[,2]))
    
    # ******************** Declare netCDF variables ********************#
    t <- ncdf4::ncdim_def(name = "time",
                          longname = "time",
                          units = paste0("days since ", y, "-01-01 00:00:00"),
                          vals = sub_dates_cf,
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
    nc_var <- list(
      "GPP" = PEcAn.utils::to_ncvar("GPP", dims),
      "NPP" = PEcAn.utils::to_ncvar("NPP", dims),
      "TotalResp" = PEcAn.utils::to_ncvar("TotalResp", dims),
      "AutoResp" = PEcAn.utils::to_ncvar("AutoResp", dims),
      "HeteroResp" = PEcAn.utils::to_ncvar("HeteroResp", dims),
      "SoilResp" = ncdf4::ncvar_def("SoilResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999,
                                    longname = "Soil Respiration"), #need to figure out standard variable for this output
      "NEE" = PEcAn.utils::to_ncvar("NEE", dims),
      "AbvGrndWood" = PEcAn.utils::to_ncvar("AbvGrndWood", dims),
      "leaf_carbon_content" = PEcAn.utils::to_ncvar("leaf_carbon_content", dims),
      "TotLivBiom" = PEcAn.utils::to_ncvar("TotLivBiom", dims),
      "TotSoilCarb" = PEcAn.utils::to_ncvar("TotSoilCarb", dims),
      "Qle" = PEcAn.utils::to_ncvar("Qle", dims),
      "Transp" = PEcAn.utils::to_ncvar("Transp", dims),
      "SoilMoist" = PEcAn.utils::to_ncvar("SoilMoist", dims),
      "SoilMoistFrac" = PEcAn.utils::to_ncvar("SoilMoistFrac", dims),
      "SWE" = PEcAn.utils::to_ncvar("SWE", dims),
      "litter_carbon_content" = PEcAn.utils::to_ncvar("litter_carbon_content", dims),
      "LAI" = PEcAn.utils::to_ncvar("LAI", dims),
      "fine_root_carbon_content" = PEcAn.utils::to_ncvar("fine_root_carbon_content", dims),
      "coarse_root_carbon_content" = PEcAn.utils::to_ncvar("coarse_root_carbon_content", dims),
      "AGB" = ncdf4::ncvar_def("AGB", units = "kg C m-2", dim = list(lon, lat, t), missval = -999,
                               longname = "Total aboveground biomass"),
      "time_bounds" = ncdf4::ncvar_def(name="time_bounds", units='',
                                       longname = "history time interval endpoints", dim=list(time_interval,time = t), 
                                       prec = "double")              
    )

    if ("litter_mass_content_of_water" %in% names(output)) {
      nc_var[["litter_mass_content_of_water"]] <- PEcAn.utils::to_ncvar("litter_mass_content_of_water", dims)
    }
    if ("GWBI" %in% names(output)) {
      nc_var[["GWBI"]] <- ncdf4::ncvar_def("GWBI", units = "kg C m-2", dim = list(lon, lat, t), missval = -999,
                                           longname = "Gross Woody Biomass Increment")
    }
    if ("mineral_N" %in% names(output)) {
      nc_var[["mineral_N"]] <- ncdf4::ncvar_def("mineral_N", units = "kg N m-2",
        dim = list(lon, lat, t), missval = -999, longname = "Soil mineral nitrogen")
    }
    if ("soil_organic_N" %in% names(output)) {
      nc_var[["soil_organic_N"]] <- ncdf4::ncvar_def("soil_organic_N", units = "kg N m-2",
        dim = list(lon, lat, t), missval = -999, longname = "Soil organic nitrogen")
    }
    if ("litter_N" %in% names(output)) {
      nc_var[["litter_N"]] <- ncdf4::ncvar_def("litter_N", units = "kg N m-2",
        dim = list(lon, lat, t), missval = -999, longname = "Litter nitrogen")
    }
    if ("N2O_flux" %in% names(output)) {
      nc_var[["N2O_flux"]] <- PEcAn.utils::to_ncvar("N2O_flux", dims)
    }
    if ("N_leaching" %in% names(output)) {
      nc_var[["N_leaching"]] <- ncdf4::ncvar_def("N_leaching", units = "kg N m-2 s-1",
        dim = list(lon, lat, t), missval = -999, longname = "Nitrogen leaching flux")
    }
    if ("N_fixation" %in% names(output)) {
      nc_var[["N_fixation"]] <- ncdf4::ncvar_def("N_fixation", units = "kg N m-2 s-1",
        dim = list(lon, lat, t), missval = -999, longname = "Nitrogen fixation flux")
    }
    if ("N_uptake" %in% names(output)) {
      nc_var[["N_uptake"]] <- ncdf4::ncvar_def("N_uptake", units = "kg N m-2 s-1",
        dim = list(lon, lat, t), missval = -999, longname = "Plant nitrogen uptake flux")
    }
    if ("CH4_flux" %in% names(output)) {
      nc_var[["CH4_flux"]] <- PEcAn.utils::to_ncvar("CH4_flux", dims)
    }
    
    # ******************** Create netCDF and output variables ********************#
    ### Output netCDF data
    if(conflicted & conflict){
      nc      <- ncdf4::nc_create(file.path(outdir, paste("current", "nc", sep = ".")), nc_var)
      ncdf4::ncatt_put(nc, "time", "bounds", "time_bounds", prec=NA)
      for (key in names(nc_var)) {
        ncdf4::ncvar_put(nc, nc_var[[key]], output[[key]])
      }
      ncdf4::nc_close(nc)
      
      #merge nc files of the same year together to enable the assimilation of sub-annual data
      if(file.exists(file.path(outdir, "previous.nc"))){
        files <- c(file.path(outdir, "previous.nc"), file.path(outdir, "current.nc"))
      }else{
        files <- file.path(outdir, "current.nc")
      }
      mergeNC(files = files, outfile = file.path(outdir, paste(y, "nc", sep = ".")))
      #The command "cdo" in mergeNC will automatically rename "time_bounds" to "time_bnds". However, "time_bounds" is used 
      #in read_restart codes later. So we need to read the new NetCDF file and convert the variable name back. 
      nc<- ncdf4::nc_open(file.path(outdir, paste(y, "nc", sep = ".")),write=TRUE)
      nc<-ncdf4::ncvar_rename(nc,"time_bnds","time_bounds")
      ncdf4::ncatt_put(nc, "time", "bounds","time_bounds", prec=NA)
      ncdf4::nc_close(nc)
      unlink(files, recursive = T)
    }else{
      nc      <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")), nc_var)
      ncdf4::ncatt_put(nc, "time", "bounds", "time_bounds", prec=NA)
      for (key in names(nc_var)) {
        ncdf4::ncvar_put(nc, nc_var[[key]], output[[key]])
      }
      ncdf4::nc_close(nc)
    }
  }  ### End of year loop
  
  ## Delete raw output, if requested
  if (delete.raw) {
    file.remove(sipnet_out_file)
  }
} # model2netcdf.SIPNET
#--------------------------------------------------------------------------------------------------#
### EOF

# Helper Function 

sipnet2datetime <- function(year, doy, hour){
  
  hr <- floor(hour)
  minsec <- PEcAn.utils::ud_convert(hour - hr, "hour", "min") 
  minute <- floor(minsec)
  
  sec <- PEcAn.utils::ud_convert(minsec - minute, "minute", "second")
  
  minute <- ifelse(sec == 60, minute + 1, minute)
  sec <- ifelse(sec == 60, 0, sec)
  
  hr <- ifelse(minute == 60, hr + 1, hr)
  minute <- ifelse(minute == 60, 0, minute)
  
  datetime <- strptime(
    paste(year, doy, hr, minute, sec),
    "%Y %j %H %M %S", 
    tz = "UTC"
  )
  
  as.POSIXct(datetime)
}






