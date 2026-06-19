##' Convert RCMEM output to netCDF
##'
##' Converts all output contained in a folder to netCDF.
##' @name model2netcdf.RCMEM
##' @title Function to convert RCMEM model output to standard netCDF format
##' @param outdir Location of RCMEM model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start year of the simulation
##' @param end_date End year of the simulation
##' @param delete_raw logical: remove out.csv after converting?
##' @export
##' @author J. Holmquist
##' 
model2netcdf.RCMEM <- function(outdir="models/rcmem/demo_run/input_demo_out/out/ENS-00001-SERC/", 
                               sitelat = 38.874544, 
                               sitelon=-76.548628, 
                               start_date, 
                               end_date, 
                               delete_raw = FALSE) {
  
  require(tidyverse)
  
  runid <- basename(outdir)
  raw_output_cohort <- file.path(outdir, "cohorts_out.csv")
  raw_output_scenario <- file.path(outdir, "scenario_out.csv")
  raw_species_scenario <- file.path(outdir, "species_out.csv")
  
  ### Read in model output in RCMEM
  RCMEM.cohort.output <- utils::read.csv(raw_output_cohort)
  RCMEM.scenario.output <- utils::read.csv(raw_output_scenario)
  RCMEM.species.output <- utils::read.csv(raw_species_scenario) %>% 
    tidyr::pivot_longer(names_to = "species_code", values_to = "aboveground_biomass", -year)
  
  # RCMEM.cohort.dims <- dim(RCMEM.cohort.output)
  # RCMEM.scenario.dims <- dim(RCMEM.scenario.output)
  # RCMEM.species.dims <- dim(RCMEM.species.output)

  years <- as.numeric(start_date):as.numeric(end_date)
  
  ### Loop over years in PEPRMT output to create separate netCDF outputs
  for (y in years) {
    if (file.exists(file.path(outdir, paste(y, "nc", sep = ".")))) {
      next
    }
    print(paste("---- Processing year: ", y))  #turn on for debugging
    
    ## Subset data for processing
    sub.RCMEM.output_cohort <- dplyr::filter(RCMEM.cohort.output, .data$year == y)
    sub.RCMEM.output_scenario <- dplyr::filter(RCMEM.scenario.output, .data$year == y)
    sub.RCMEM.output_species <- dplyr::filter(RCMEM.species.output, .data$year == y) %>% 
      dplyr::mutate(species_index = 1:n())
  
    # sub.RCMEM.output_cohort.dims <- dim(sub.RCMEM.output_cohort)
    # sub.RCMEM.output_scenario.dims <- dim(sub.RCMEM.output_scenario)
    # sub.RCMEM.output_species.dims <- dim(sub.RCMEM.output_species)

    # ******************** Declare netCDF variables ********************#
    # start.day <- 1
    # if (y == lubridate::year(start_date)){
    #   start.day <- lubridate::yday(start_date)
    # } 
    
    tvals <- c(1)
    
    # bounds[,1] <- tvals
    # bounds[,2] <- bounds[,1]+365
    t   <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", y, "-01-01 00:00:00"), 
                            vals = tvals, calendar = "standard", unlim = TRUE)
    ## ***** Need to dynamically update the UTC offset here *****
    
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")
    
    depth <- ncdf4::ncdim_def("depth", 
                                     "cm", 
                                     vals = as.numeric(sub.RCMEM.output_cohort[,"cumVol"]),
                                     longname = "Depth from surface")
    
   
    pft <- ncdf4::ncdim_def(
        name = "pft",
        units = "unitless",
        vals = as.numeric(sub.RCMEM.output_species[,"species_index"]),
        longname = "Plant Functional Type",
        unlim = TRUE
      )
    
    dims_scenario <- list(lon = lon, lat = lat, time = t)
    dims_cohorts <- list(lon = lon, lat = lat, depth = depth,  time = t)
    dims_species <- list(lon = lon, lat = lat, pft = pft,  time = t)
    
    # What is this below???
    # time_interval <- ncdf4::ncdim_def(name = "hist_interval", 
    #                                   longname="history time interval endpoint dimensions",
    #                                   vals = 1:2, units="")
    
    # sub.PEPRMT.output <- sub.PEPRMT.output[c(fluxes, pools)]
    
    ## Setup outputs for netCDF file in appropriate units
    output <- list()
    
    ## cohorts
    #  "cohort_index"          "mineral"               "fast"                 
    # [5] "slow"                  "root"                  "vol"                   "nonRootVol"           
    # [9] "cumVol"                "inputYrs"              "omPackingDensity"      "mineralPackingDensity"
    
    output[[1]] <- sub.RCMEM.output_cohort[,"mineral"]
    output[[2]] <- sub.RCMEM.output_cohort[,"fast"]
    output[[3]] <- sub.RCMEM.output_cohort[,"slow"]
    output[[4]] <- sub.RCMEM.output_cohort[,"root"]
    
    # z_min 
    # output[[6]] <- sub.RCMEM.output_cohort[,"vol"]
    # output[[7]] <- sub.RCMEM.output_cohort[,"cumVol"] # z_top
    # z_bottom
    output[[5]] <- sub.RCMEM.output_cohort[,"inputYrs"]
    output[[6]] <- sub.RCMEM.output_cohort[,"omPackingDensity"]
    output[[7]] <- sub.RCMEM.output_cohort[,"mineralPackingDensity"]
    
    # Scenario
    # [1] "year"                "years_per_iteration" "surface_elevation"   "aboveground_biomass"
    # [5] "belowground_biomass" "totalRootVolume"     "sediment_delivered"  "rootToShoot"        
    # [9] "rootTurnover"        "abovegroundTurnover" "rootDepthMax"        "rootPackingDensity" 
    # [13] "lambda"              "rootShape"
    
    output[[8]] <- sub.RCMEM.output_scenario[,"surface_elevation"]
    output[[9]] <- sub.RCMEM.output_scenario[,"belowground_biomass"]
    output[[10]] <- sub.RCMEM.output_scenario[,"sediment_delivered"]
    
    # Species
    # "species_code"        "aboveground_biomass"
    output[[11]] <- sub.RCMEM.output_species[,"aboveground_biomass"]

    ## time_bounds
    # output[[26]] <- c(lubridate::ymd(paste(start_date, "01", "01", sep="-")),
    #                   lubridate::ymd(paste(start_date, "12", "31", sep="-"))
    #                   )
    
    ## missing value handling
    # for (i in seq_along(output)) {
    #   if (length(output[[i]]) == 0) 
    #     output[[i]] <- rep(-999, length(t$vals))
    # }
    # 
    
    ## setup nc file
    # ******************** Declar netCDF variables ********************#
    nc_var <- list()
    nc_var[[1]]  <- ncdf4::ncvar_def(name = "mineral", units = "g", longname = "cohort mineral dry mass", dim = dims_cohorts, NA, prec = "double") 
    nc_var[[2]] <- ncdf4::ncvar_def(name = "fast", units = "g", longname = "cohort fast decaying organic matter dry mass", dim = dims_cohorts, NA, prec = "double")
    nc_var[[3]] <- ncdf4::ncvar_def(name = "slow", units = "g", longname = "cohort slow decaying organic matter dry mass", dim = dims_cohorts, NA, prec = "double")
    nc_var[[4]] <- ncdf4::ncvar_def(name = "root", units = "g", longname = "cohort live root dry mass", dim = dims_cohorts, NA, prec = "double")
    
    nc_var[[5]] <- ncdf4::ncvar_def(name = "inputYrs", units = "years", longname = "cohort input years", dim = dims_cohorts, NA, prec = "double")
    nc_var[[6]] <- ncdf4::ncvar_def(name = "omPackingDensity", units = "g cm-3", longname = "cohort organic matter packing density", dim = dims_cohorts, NA, prec = "double")
    nc_var[[7]] <- ncdf4::ncvar_def(name = "mineralPackingDensity", units = "g cm-3", longname = "cohort mineral packing density", dim = dims_cohorts, NA, prec = "double")
    
    nc_var[[8]] <- ncdf4::ncvar_def(name = "surface_elevation", units = "cm", longname = "cohort live root dry mass", dim = dims_scenario, NA, prec = "double")
    nc_var[[9]] <- ncdf4::ncvar_def(name = "belowground_biomass", units = "g cm-2", longname = "belowground biomass per unit area", dim = dims_scenario, NA, prec = "double")
    nc_var[[10]] <- ncdf4::ncvar_def(name = "sediment_delivered", units = "g cm-2", longname = "sediment captured in one year", dim = dims_scenario, NA, prec = "double")
    
    nc_var[[11]] <- ncdf4::ncvar_def(name = "aboveground_biomass", units = "g cm-2", longname = "aboveground herbacious biomass", dim = dims_species, NA, prec = "double")
    
    
    ### Output netCDF data
    nc <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")), nc_var)
    ncdf4::ncatt_put(nc, "time", "bounds", "time_bounds", prec=NA)
    for (i in seq_along(nc_var)) {
      ncdf4::ncvar_put(nc, nc_var[[i]], output[[i]])
    }
    ncdf4::nc_close(nc)

  }  ### End of year loop

  if (delete_raw) {
    file.remove(raw_output)
  }
} # model2netcdf.RCMEM
# ==================================================================================================#
## EOF
