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
                               start_date=1928, 
                               end_date=2018, 
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
    tidyr::pivot_longer(names_to = "species_code", values_to = "species_aboveground_biomass", -year)
  
  # RCMEM.cohort.dims <- dim(RCMEM.cohort.output)
  # RCMEM.scenario.dims <- dim(RCMEM.scenario.output)
  # RCMEM.species.dims <- dim(RCMEM.species.output)

  years <- start_date:end_date
  
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
    
    # 
    # sub.RCMEM.output_cohort.dims <- dim(sub.RCMEM.output_cohort)
    # sub.RCMEM.output_scenario.dims <- dim(sub.RCMEM.output_scenario)
    # sub.RCMEM.output_species.dims <- dim(sub.RCMEM.output_species)
    # 

    # ******************** Declare netCDF variables ********************#
    # start.day <- 1
    # if (y == lubridate::year(start_date)){
    #   start.day <- lubridate::yday(start_date)
    # } 
    tvals <- c(1)
     
    bounds <- array(data=NA, dim=c(length(tvals),2))
    bounds[,1] <- tvals
    bounds[,2] <- bounds[,1]+364
    
    t   <- ncdf4::ncdim_def(name = "cal_date_mid", units = paste0("yr, mon, day, hr, min, sec"),
                     vals = lubridate::ymd_hms(paste(y, "06", "15", "12", "00", "00", sep = "-")), calendar = "standard", unlim = TRUE)

    
    ## ***** Need to dynamically update the UTC offset here *****
    
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")
    
    depth <- ncdf4::ncdim_def("depth", 
                                     "m", 
                                     vals = as.numeric(sub.RCMEM.output_cohort[,"cumVol"])/100,
                                     longname = "Depth")
    
   
    pft <- ncdf4::ncdim_def(
        name = "pft",
        units = "unitless",
        vals = as.numeric(sub.RCMEM.output_species[,"species_index"]),
        longname = "Plant Functional Type",
        unlim = TRUE
      )
    
    dims_scenario <- list(lon = lon, lat = lat)
    dims_cohorts <- list(lon = lon, lat = lat, depth = depth)
    dims_species <- list(lon = lon, lat = lat, pft = pft)
    
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
    #output[[5]] <- sub.RCMEM.output_cohort[,"vol"]
    #output[[7]] <- sub.RCMEM.output_cohort[,"cumVol"] # z_top
    # z_bottom
    output[[8]] <- sub.RCMEM.output_cohort[,"inputYrs"]
    output[[9]] <- sub.RCMEM.output_cohort[,"omPackingDensity"]
    output[[10]] <- sub.RCMEM.output_cohort[,"mineralPackingDensity"]
    
    # Scenario
    # [1] "year"                "years_per_iteration" "surface_elevation"   "aboveground_biomass"
    # [5] "belowground_biomass" "totalRootVolume"     "sediment_delivered"  "rootToShoot"        
    # [9] "rootTurnover"        "abovegroundTurnover" "rootDepthMax"        "rootPackingDensity" 
    # [13] "lambda"              "rootShape"
    
    output[[12]] <- sub.RCMEM.output_scenario[,"surface_elevation"]
    output[[14]] <- sub.RCMEM.output_scenario[,"belowground_biomass"]
    output[[16]] <- sub.RCMEM.output_scenario[,"sediment_delivered"]
    
    # Species
    # "species_code"        "aboveground_biomass"
    output[[25]] <- sub.RCMEM.output_species[,"aboveground_biomass"]

        ## time_bounds
    output[[26]] <- c(lubridate::ymd(paste(start_date, "01", "01", sep="-")),
                      lubridate::ymd(paste(start_date, "12", "31", sep="-"))
                      )
    
    ## missing value handling
    # for (i in seq_along(output)) {
    #   if (length(output[[i]]) == 0) 
    #     output[[i]] <- rep(-999, length(t$vals))
    # }
    # 
    
    ## setup nc file
    # ******************** Declar netCDF variables ********************#
    nc_var <- list()
    nc_var[[1]]  <- PEcAn.utils::to_ncvar("mineral", dims_cohorts)
    
    # nc_var[[1]]  <- PEcAn.utils::to_ncvar("CH4_flux", dims)
    # nc_var[[2]]  <- PEcAn.utils::to_ncvar("GPP", dims)
    # nc_var[[3]]  <- PEcAn.utils::to_ncvar("TotalResp", dims)
    # nc_var[[4]]  <- PEcAn.utils::to_ncvar("NEE", dims)
    # 
    # nc_var[[5]]  <- PEcAn.utils::to_ncvar("slow_soil_pool_carbon_content", dims)
    # nc_var[[6]]  <- PEcAn.utils::to_ncvar("fast_soil_pool_carbon_content", dims)
    
    # nc_var[[7]] <- ncdf4::ncvar_def(name="time_bounds", units='', 
    #                                  longname = "history time interval endpoints", dim=list(time_interval,time = t), 
    #                                  prec = "double")
    
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
} # model2netcdf.PEPRMT
# ==================================================================================================#
## EOF
