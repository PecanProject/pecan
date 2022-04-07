#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Convert STICS output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.STICS
##' @title Code to convert STICS' output into netCDF format
##'
##' @param outdir Location of model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @param overwrite Whether or not to overwrite existing output files 
##' @export
##'
##' @author Istem Fer
##' 
model2netcdf.STICS <- function(outdir, sitelat, sitelon, start_date, end_date, overwrite = FALSE) {
  
  ### Read in model output in STICS format
  out_files <- list.files(outdir)
  
  stics_out_file <- file.path(outdir, out_files[grepl("mod_s.*", out_files)])
  stics_output   <- lapply(stics_out_file, read.table, header = TRUE, sep = ";")
  stics_output  <- do.call("rbind", stics_output)
  # probably already ordered, but order by year and DoY
  stics_output <-  stics_output[order(stics_output[,1], stics_output[,4]), ]

  
  simulation_years <- unique(stics_output$ian)
  
  # get all years that we want data from
  year_seq <- seq(lubridate::year(start_date), lubridate::year(end_date))
  
  # check that specified years and output years match
  if (!all(year_seq %in% simulation_years)) {
    # if not fail altogether, so that it won't break ensemble analysis 
    PEcAn.logger::logger.error("Years selected for model run and STICS output years do not match.")
  }
  
  # determine time step?
  
  for (y in simulation_years) {
    
    if (file.exists(file.path(outdir, paste(y, "nc", sep = "."))) & overwrite == FALSE) {
      next
    }
    
    thisyear <- stics_output[ , "ian"] == y
    
    outlist <- list()
    outlist[[1]] <- stics_output[thisyear, "lai.n."]  # LAI in (m2 m-2)
    
    # daily amount of CO2-C emitted due to soil mineralisation (humus and organic residues) (kg ha-1 d-1)
    HeteroResp <- udunits2::ud.convert(stics_output[thisyear, "CO2sol"],  "ha-1 day-1", "m-2 s-1")
    
    outlist[[2]] <- HeteroResp
    
    # AutoResp = AbvResp + RootResp
    
    # following a similar approach in LPJ-GUESS
    # AbvResp = ABG * respcoeff * g(T) following Sun et al., 2007 doi: 10.1007/s00376-007-0055-4
    # masec_kg_ha: Aboveground dry matter (kg ha-1)
    # CNplante: N concentration in the aboveground plant (% dry weight)
    
    # assumptions ---
    # respcoeff: need some sort of maintenance respiration coefficient parameter, there isn't one in STICS (double check), also should it differ between plant stages?
    # also this coefficient should differ between plant stages, assuming this will be driven by N
    # instead of using ABG, I'll use C_abv / CN_abv to account for this (similar to LPJ-GUESS?), so new eqn:*
    # AbvResp = (C_abv / CN_abv) * respcoeff * g(T)
    # using leaf_maintenance_respiration_mass variable from BetyDB (umol [CO2] kg-1 s-1) for this respcoeff
    # no MR rate parameter really have kg N-1 in units but as I'm including N effect here and assume units
    
    # resp par hacks
    resppar_file <- file.path(outdir, "RespPars.Rdata")
    if(file.exists(resppar_file)){
      load(resppar_file)
    }else{
      leaf_maintenance_respiration_mass <- 9000 # hardcoding for development, need a prior on it, note that prior will need to be higher than normal (x ~100)
      # or pass the normal prior vals but scale it here somehow (maybe with another prior)?
      leaf_respiration_Q10 <- 2.8 # hardcoding for development, need a prior on it
    }

    #PEcAn.logger::logger.info("Values for leaf_maintenance_respiration_mass and leaf_respiration_Q10 are:", leaf_maintenance_respiration_mass, leaf_respiration_Q10)
    
    # g(T): respiration temperature response function, some Q10 equation, is there something similar in STICS?
    # gtemp = Q10 ^ ([T-25]/10) for testing using Sun et al.
    # Q10: using leaf_respiration_Q10 from BetyDB
    # tcult: crop surface temperature (daily average) degreeC

    gtemp <- leaf_respiration_Q10 ^ ((stics_output[thisyear, "tcult"] - 25) / 10)
    leafC <- 0.48 # is this calculated by STICS? there is something like Crac: amount of C in roots at harvest
    AbvResp <- (stics_output[thisyear, "masec_kg_ha"] * leafC * (stics_output[thisyear, "CNplante"])/100) * leaf_maintenance_respiration_mass * gtemp
    # (kg ha-1) * (umol [CO2] kg-1 s-1) 
    #  now in umol CO2 ha-1 s-1, pecan units are in kg C m-2 s-1  
    AbvResp <- udunits2::ud.convert((AbvResp / (44/12)), "ha-1", "m-2")
    # now umol C to kg C
    AbvResp <- PEcAn.utils::misc.convert(AbvResp, "umol C m-2 s-1", "kg C m-2 s-1")
    
    RootResp <- AbvResp * 1.5 # factor from Moureaux for now
    ## RootResp = (C_root/ CN_root) * r * g(Tsoil) can use something like this explicitly
    
    MaintResp <- AbvResp + RootResp # kg C m-2 s-1  
    
    # deriving GPP:  (GPP - MR)*0.72 = NPP
    
    # dltams(n): daily growth rate of the plant (t.ha-1.d-1) 
    dltams      <- udunits2::ud.convert(stics_output[thisyear, "dltams.n."], "ton", "kg") * 0.48 # ton to kgC
    # dltaremobil: daily amount of perennial reserves remobilised (t.ha-1.d-1)
    dltaremobil <- udunits2::ud.convert(stics_output[thisyear, "dltaremobil"], "ton", "kg") * 0.48 # ton to kgC

    NPP <- dltams - dltaremobil # kgC ha-1 d-1
    NPP[NPP<0] <- 0
    
    # double checking that this is all NPP (above and below)
    ## this
    #stics_output[thisyear, "dltams.n."]  # t.ha-1.d-1
    ##should be roughly equal to this
    #diff(stics_output[thisyear, "masec.n."])+ diff(stics_output[thisyear, "msrac.n."]) # t.ha-1
    
    NPP <- udunits2::ud.convert(NPP, "ha-1 day-1", "m-2 s-1") # kg C m-2 s-1  
    outlist[[3]] <- NPP
    
    GPP <- (NPP / 0.72) + MaintResp
    outlist[[4]] <- GPP
    
    # deriving Growth respiration:  (GPP - MR)*0.28 = GR where 0.28 comes from ORCHIDEE-STICS 10.1016/j.agee.2016.04.017
    GrowthResp <- (GPP - MaintResp)*0.28
    
    AutoResp <- MaintResp + GrowthResp
    outlist[[5]] <- AutoResp
    
    TotalResp <- AutoResp + HeteroResp
    outlist[[6]] <- TotalResp
    
    NEE <- -1*(GPP-TotalResp)
    outlist[[7]] <- NEE
      

    
    # ******************** Declare netCDF dimensions and variables ********************#
    t <- ncdf4::ncdim_def(name = "time", 
                          units = paste0("days since ", y, "-01-01 00:00:00"), 
                          stics_output[stics_output[,1] == y, 4], # allow partial years, this info is already in matrix_weather
                          calendar = "standard", 
                          unlim = TRUE)
    
    
    lat <- ncdf4::ncdim_def("lat", "degrees_north", vals = as.numeric(sitelat), longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east",  vals = as.numeric(sitelon), longname = "station_longitude")
    
    dims <- list(lon = lon, lat = lat, time = t)
    
    nc_var <- list()
    nc_var[[1]] <- PEcAn.utils::to_ncvar("LAI", dims)
    nc_var[[2]] <- PEcAn.utils::to_ncvar("HeteroResp", dims)
    nc_var[[3]] <- PEcAn.utils::to_ncvar("NPP", dims)
    nc_var[[4]] <- PEcAn.utils::to_ncvar("GPP", dims)
    nc_var[[5]] <- PEcAn.utils::to_ncvar("AutoResp", dims)
    nc_var[[6]] <- PEcAn.utils::to_ncvar("TotalResp", dims)
    nc_var[[7]] <- PEcAn.utils::to_ncvar("NEE", dims)
    
    # ******************** Declare netCDF variables ********************#
    
    ### Output netCDF data
    nc <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")), nc_var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep = ".")), "w")
    for (i in seq_along(nc_var)) {
      # print(i)
      ncdf4::ncvar_put(nc, nc_var[[i]], outlist[[i]])
      cat(paste(nc_var[[i]]$name, nc_var[[i]]$longname), file = varfile, sep = "\n")
    }
    close(varfile)
    ncdf4::nc_close(nc)
    
  } ### End of year loop

  
} # model2netcdf.STICS
