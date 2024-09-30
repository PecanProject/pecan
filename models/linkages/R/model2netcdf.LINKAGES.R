#' Convert LINKAGES's output into netCDF format
#'
#' Convert MODEL output into the NACP Intercomparison format (ALMA using netCDF)
#'
#' @param outdir Location of model output
#' @param sitelat Latitude of the site
#' @param sitelon Longitude of the site
#' @param start_date Start time of the simulation
#' @param end_date End time of the simulation
#' @param pft_names names of PFTs to use in output labels
#' @export
#'
#' @author Ann Raiho, Betsy Cowdery
#'
model2netcdf.LINKAGES <- function(outdir, sitelat, sitelon, start_date = NULL,
                                  end_date = NULL, pft_names = NULL) {
  # , PFTs) { logger.severe('NOT IMPLEMENTED')
  
  ### Read in model output in linkages format
  load(file.path(outdir, "linkages.out.Rdata"))
  # linkages.output.dims <- dim(linkages.output)
  
  ### Determine number of years and output timestep
  
  start_year <- as.numeric(strftime(start_date, "%Y"))
  end_year <- as.numeric(strftime(end_date, "%Y"))
  years <- start_year:end_year
  
  # IF : no need this, write.configs --> job.sh --> will pass model2netcsf.LINKAGES pft_names
  # if(is.null(pft_names)){
  #   pft_names <- as.character(1:length(agb.pft[, 1, 1]))
  # }
  
  ### Loop over years in linkages output to create separate netCDF outputs
  for (y in seq_along(years)) {
    if (file.exists(file.path(outdir, paste(years[y], "nc", sep = ".")))) {
      next
    }
    print(paste("---- Processing year: ", years[y]))  # turn on for debugging
    
    ## Subset data for processing sub.linkages.output <- subset(linkages.output, year == y)
    ## sub.linkages.output.dims <- dim(sub.linkages.output)
    
    # sub.linkages.pft <- subset(linkages.pft, year == y) sub.linkages.pft.dims <-
    # dim(sub.linkages.pft)
    
    ## Setup outputs for netCDF file in appropriate units
    output <- list()
    ## standard variables: Carbon Pools
    output[[1]]  <- ag.biomass[y, ]  # Above Ground Biomass in kgC/m2
    output[[2]]  <- ag.biomass[y, ]  # Total Live Biomass in kgC/m2 (no distinction from AGB in linkages)
    output[[3]]  <- total.soil.carbon[y, ]  # TotSoilCarb in kgC/m2
    output[[4]]  <- c(ag.biomass[y, ], total.soil.carbon[y, ], leaf.litter[y, ], area[y, ])  # Carb Pools in kgC/m2
    output[[5]]  <- c("AGB", "Soil Organic Matter", "Leaf Litter", "LAI")  # poolname
    output[[6]]  <- ag.npp[y, ]  # GWBI = NPP in linkages
    output[[7]]  <- hetero.resp[y, ]  # HeteroResp in kgC/m^2/s
    output[[8]]  <- ag.npp[y, ]  # NPP = GWBI in linkages
    output[[9]]  <- nee[y, ]  # NEE #possibly questionable
    output[[10]] <- et[y, ]  # Evap in kg/m^2/s
    output[[11]] <- agb.pft[, y, ]
    output[[12]] <- f.comp[, y]
    output[[13]] <- area[y, ]  #LAI
    output[[14]] <- water[y, ]  #soil moisture
    output[[15]] <- abvgroundwood.biomass[y,] #AbvGroundWood just wood no leaves
    output[[16]] <- seq_along(pft_names) 
    
    # ******************** Declare netCDF variables ********************#
    dim.t <- ncdf4::ncdim_def(name = "time", 
                       units = paste0("days since ", years[y], "-01-01 00:00:00"), 
                       vals = 0, calendar = "standard", 
                       unlim = TRUE)
    dim.lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    dim.lon <- ncdf4::ncdim_def("lon", "degrees_east", vals = as.numeric(sitelon), longname = "station_longitude")
    dim.string <- ncdf4::ncdim_def("names", "", 1:24, create_dimvar = FALSE)
    dim.cpools <- ncdf4::ncdim_def("cpools", "", vals = 1:4, longname = "Carbon Pools")
    dim.cpools1 <- ncdf4::ncdim_def("cpools", "", vals = 1:4, longname = "Carbon Pools", create_dimvar = FALSE)
    #dim.pfts <- ncdim_def("pfts", "", vals = 1:nrow(agb.pft), longname = "PFTs", create_dimvar = FALSE)
    dim.pfts <- ncdf4::ncdim_def(name = "pft", units = "unitless", vals = 1:length(agb.pft[, 1, 1]), longname = "Plant Functional Type", unlim = TRUE)
    
    
    for (i in seq_along(output)) {
      if (length(output[[i]]) == 0) 
        output[[i]] <- rep(-999, length(dim.t$vals))
    }
    
    dims <- list(lon = dim.lon, lat = dim.lat, time = dim.t)
    
    nc_var <- list()
    nc_var[[1]]  <- PEcAn.utils::to_ncvar("AGB", dims)
    nc_var[[2]]  <- PEcAn.utils::to_ncvar("TotLivBiom", dims)
    nc_var[[3]]  <- PEcAn.utils::to_ncvar("TotSoilCarb", dims)
    nc_var[[4]]  <- ncdf4::ncvar_def("CarbPools", "kgC/m2", list(dim.cpools, dim.lat, dim.lon, dim.t), -999)
    nc_var[[5]]  <- ncdf4::ncvar_def("poolnames", units = "", dim = list(dim.string, dim.cpools1), longname = "Carbon Pool Names", prec = "char")
    nc_var[[6]]  <- ncdf4::ncvar_def("GWBI", "kgC/m2", list(dim.lat, dim.lon, dim.t), -999)
    nc_var[[7]]  <- ncdf4::ncvar_def("HeteroResp", "kgC/m2/s", list(dim.lat, dim.lon, dim.t), -999)
    nc_var[[8]]  <- ncdf4::ncvar_def("NPP", "kgC/m2/s", list(dim.lat, dim.lon, dim.t), -999)
    nc_var[[9]]  <- ncdf4::ncvar_def("NEE", "kgC/m2/s", list(dim.lat, dim.lon, dim.t), -999)
    nc_var[[10]] <- ncdf4::ncvar_def("Evap", "kg/m2/s", list(dim.lat, dim.lon, dim.t), -999)
    nc_var[[11]] <- ncdf4::ncvar_def("AGB.pft", "kgC/m2", list(dim.lat, dim.lon, dim.t, dim.pfts), -999)
    nc_var[[12]] <- ncdf4::ncvar_def("Fcomp", "kgC/kgC", list(dim.lat, dim.lon, dim.t, dim.pfts), -999)
    nc_var[[13]] <- ncdf4::ncvar_def("LAI", "m2/m2", list(dim.lat, dim.lon, dim.t), -999)
    nc_var[[14]] <- ncdf4::ncvar_def("SoilMoist", "m2/m2", list(dim.lat, dim.lon, dim.t), -999)
    nc_var[[15]] <- ncdf4::ncvar_def("AbvGrndWood", "kgC/m2", list(dim.lat, dim.lon, dim.t), -999)
    nc_var[[16]] <- ncdf4::ncvar_def("PFT", units = "", dim = list(dim.pfts),  
                     longname = paste(pft_names, collapse=","))
    
    # ******************** Declare netCDF variables ********************#
    
    ### Output netCDF data
    nc <- ncdf4::nc_create(file.path(outdir, paste(formatC(years[y], width = 4, format = "d", flag = "0"), "nc", sep = ".")), nc_var)
    varfile <- file(file.path(outdir, paste(formatC(years[y], width = 4, format = "d", flag = "0"), "nc", "var", sep = ".")), "w")
   
     for (i in seq_along(nc_var)) {
      print(i)
      ncdf4::ncvar_put(nc, nc_var[[i]], output[[i]])
      cat(paste(nc_var[[i]]$name, nc_var[[i]]$longname), file = varfile, sep = "\n")
    }
    close(varfile)
    ncdf4::nc_close(nc)
    
  }  ### End of year loop
} # model2netcdf.LINKAGES
