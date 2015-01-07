#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Convert MODEL output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.MODEL
##' @title Code to convert MODELS's output into netCDF format
##'
##' @param outdir Location of model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##'
##' @author Rob Kooper
model2netcdf.LINKAGES <- function(outdir, sitelat, sitelon, start_date=NULL, end_date=NULL,force=FALSE) {
#  logger.severe("NOT IMPLEMENTED")
  
  ### Read in model output in LINKAGES format
  output <- as.matrix(read.csv(file.path(outdir, "OUT.csv"), header=FALSE))
  output = na.omit(output)
  output.dims <- dim(output)    
  block.size = round(output.dims[1]/4)
  LINKAGES.output = matrix(numeric(0),block.size,output.dims[2]*4 -3)
  LINKAGES.output[,1:output.dims[2]] = output[1:block.size,]
  LINKAGES.output[,(1:(output.dims[2]-1)) + output.dims[2] ] = output[(1:block.size) + block.size,-1]
  LINKAGES.output[,(1:(output.dims[2]-1)) + 2*output.dims[2] -1 ] = output[1+(1:block.size) + 2*block.size,-1]
  LINKAGES.output[,(1:(output.dims[2]-1)) + 3*output.dims[2] -2 ] = output[1+(1:block.size) + 3*block.size,-1]
  
  colnames(LINKAGES.output) <- c("year","numStems","agBiomass","leafLitter","leafLitterN","agNPP","availN","humusCN","soilResp","soilOM","ET",
                                 "numStems.SD","agBiomass.SD","leafLitter.SD","leafLitterN.SD","agNPP.SD","availN.SD","humusCN.SD","soilResp.SD","soilOM.SD","ET.SD",
                                  paste0("spp",output[block.size*2+1,-1]),                                  
                                  paste0("spp",output[block.size*2+1,-1],".SD"))
  LINKAGES.output <- as.data.frame(LINKAGES.output)
    
  ### Loop over years in LINKAGES output to create separate netCDF outputs
  for (y in LINKAGES.output$year){
    if (file.exists(file.path(outdir, paste(y,"nc", sep="."))) & force == FALSE) {
      next
    }
    print(paste("---- Processing year: ", y))  # turn on for debugging
    
    ## Subset data for processing
    sub.LINKAGES.output <- subset(LINKAGES.output, year == y)
    sub.LINKAGES.output.dims <- dim(sub.LINKAGES.output)
    
    DEFAULT.C <- 0.48  ## mass percent C of biomass
    PLOT.AREA <- 830 ## m^2
    toKG <- 1000 ## Kg in Mg
    yearSecs <- (3.15569 * 10^7)
    
    ## Setup outputs for netCDF file in appropriate units
    output <- list()
    ## standard variables: Carbon Pools
    output[[1]] <- (sub.LINKAGES.output$agBiomass / PLOT.AREA * DEFAULT.C * toKG) # Above Ground Biomass in kgC/m2
    output[[2]] <- (sub.LINKAGES.output$agBiomass / PLOT.AREA * DEFAULT.C * toKG) # Total Live Biomass in kgC/m2 (no distinction from AGB in LINKAGES)
    output[[3]] <- (sub.LINKAGES.output$leafLitter + sub.LINKAGES.output$soilOM) / PLOT.AREA * DEFAULT.C * toKG # TotSoilCarb in kgC/m2
    output[[4]] <- c(sub.LINKAGES.output$agBiomass,sub.LINKAGES.output$leafLitter,sub.LINKAGES.output$soilOM) / PLOT.AREA * DEFAULT.C * toKG #Carb Pools in kgC/m2
    output[[5]] <- c("AGB","leaf Litter","Soil Organic Matter") #poolname
    output[[6]] <- (sub.LINKAGES.output$agNPP / PLOT.AREA * DEFAULT.C * toKG) # GWBI = NPP in LINKAGES
    output[[7]] <- (sub.LINKAGES.output$soilResp / PLOT.AREA / yearSecs * toKG) # HeteroResp in kgC/m^2/s
    output[[8]] <- (sub.LINKAGES.output$agNPP / PLOT.AREA * DEFAULT.C * toKG) # NPP = GWBI in LINKAGES
    output[[9]] <- ((sub.LINKAGES.output$agNPP - sub.LINKAGES.output$soilResp) / PLOT.AREA * DEFAULT.C * toKG) # NEE #possibly questionable
    output[[10]] <- (sub.LINKAGES.output$ET * yearSecs) # Evap in kg/m^2/s
    
    #******************** Declare netCDF variables ********************#
    dim.t <- ncdim_def(name = "time",
                   units = paste0("days since ", y, "-01-01 00:00:00"),
                   vals = 1:nrow(sub.LINKAGES.output),
                   calendar = "standard", unlim = TRUE)
    dim.lat <- ncdim_def("lat", "degrees_east",
                     vals =  as.numeric(sitelat),
                     longname = "station_latitude") 
    dim.lon <- ncdim_def("lon", "degrees_north",
                     vals = as.numeric(sitelon),
                     longname = "station_longitude")
    dim.string <- ncdim_def("names", "", 1:24, create_dimvar=FALSE)
    dim.cpools <- ncdim_def("cpools", "",
                        vals = 1:3,
                        longname = "Carbon Pools") 
    dim.cpools1 <- ncdim_def("cpools", "",
                         vals = 1:3,
                         longname = "Carbon Pools", create_dimvar=FALSE)   
        
    for(i in 1:length(output)){
      if(length(output[[i]])==0) output[[i]] <- rep(-999,length(t$vals))
    }
    
    var <- list()
    var[[1]]  <- ncvar_def("AGB", "kgC/m2",list(dim.lat, dim.lon, dim.t),-999)
    var[[2]]  <- ncvar_def("TotLivBiomass", "kgC/m2", list(dim.lat, dim.lon, dim.t), -999)
    var[[3]]  <- ncvar_def("TotSoilCarb", "kgC/m2", list(dim.lat, dim.lon, dim.t), -999)
    var[[4]]  <- ncvar_def("CarbPools", "kgC/m2", list(dim.cpools, dim.lat, dim.lon, dim.t),-999) 
    var[[5]]  <- ncvar_def("poolnames", units="", dim=list(dim.string, dim.cpools1), longname="Carbon Pool Names", prec="char")
    var[[6]]  <- ncvar_def("GWBI", "kgC/m2", list(dim.lat, dim.lon, dim.t), -999)
    var[[7]]  <- ncvar_def("HeteroResp", "kgC/m2/s", list(dim.lat, dim.lon, dim.t), -999)
    var[[8]]  <- ncvar_def("NPP", "kgC/m2", list(dim.lat, dim.lon, dim.t), -999)
    var[[9]]  <- ncvar_def("NEE", "kgC/m2", list(dim.lat, dim.lon, dim.t), -999)
    var[[10]]  <- ncvar_def("Evap", "kgC/m2", list(dim.lat, dim.lon, dim.t), -999)
    
    
    
    #******************** Declar netCDF variables ********************#
    
    
    ### Output netCDF data
    nc <- nc_create(file.path(outdir, paste(y,"nc", sep=".")), var)
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep=".")), "w")
    for(i in 1:length(var)){
      #print(i)
      ncvar_put(nc,var[[i]],output[[i]])  
      cat(paste(var[[i]]$name, var[[i]]$longname), file=varfile, sep="\n")
    }
    close(varfile)
    nc_close(nc)
    
  } ### End of year loop
  
  
  
}
