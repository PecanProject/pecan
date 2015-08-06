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
##' @author Ann Raiho # I changed this a bunch to do the MIP runs... Might need to change it back. Didn't commit additions for MIP runs.
model2netcdf.LINKAGES <- function(outdir, sitelat, sitelon, start_date=NULL, end_date=NULL,force=FALSE, PFTs) {
#  logger.severe("NOT IMPLEMENTED")
  #PFTs = c("acer","betula","carya","castanea.dentata","fagus.grandifolia","picea","pinus","tsuga.canadensis","quercus")
  
  ### Read in model output in LINKAGES format
  output <- as.matrix(read.csv(file.path(outdir, "OUT.csv"), header=FALSE))
  
  output[is.na(output)]<-0
  
  output.dims <- dim(output)    
  block.size = round(output.dims[1]/4)-1 #change if you change kprint
  LINKAGES.output = matrix(numeric(0),block.size,output.dims[2]*2 - 1)
  LINKAGES.output[,1:output.dims[2]] = output[1:block.size,]
  LINKAGES.output[,(1:(output.dims[2]-1)) + output.dims[2] ] = output[(1:block.size) + block.size,-1]
 
  LINKAGES.pft = matrix(numeric(0),block.size,output.dims[2]*2 - 3)
  LINKAGES.pft[,1:(output.dims[2] - 1) ] = output[1+(1:block.size) + 2*block.size,c(-11)]
  LINKAGES.pft[,(1:(output.dims[2] -2)) + (output.dims[2] - 1) ] = output[1+(1:block.size) + 3*block.size,c(-1,-11)]
  
  #LINKAGES.output = LINKAGES.output[,-c(31,41)]
  
  colnames(LINKAGES.output) <- c("year","numStems","agBiomass","leafLitter","leafLitterN","agNPP","availN","humusCN","soilResp","soilOM","ET",
                                 "numStems.SD","agBiomass.SD","leafLitter.SD","leafLitterN.SD","agNPP.SD","availN.SD","humusCN.SD","soilResp.SD","soilOM.SD","ET.SD")
  LINKAGES.output <- as.data.frame(LINKAGES.output)
  LINKAGES.output$year <- seq(0,1160,2)
  
  colnames(LINKAGES.pft) <- c("year",paste0("pft.",PFTs), paste0("pft.",PFTs,".SD"))
  LINKAGES.pft <- as.data.frame(LINKAGES.pft)
  LINKAGES.pft$year <- seq(0,1160,2)
    
  ### Loop over years in LINKAGES output to create separate netCDF outputs
  for (y in LINKAGES.output$year[2:length(LINKAGES.output$year)]){
    year_vec = seq(850,2010,1)
    
    if (file.exists(file.path(outdir, paste(year_vec[y],"nc", sep="."))) & force == FALSE) {
      next
    }
    print(paste("---- Processing year: ", year_vec[y]))  # turn on for debugging
    
    ## Subset data for processing
    sub.LINKAGES.output <- subset(LINKAGES.output, year == y)
    sub.LINKAGES.output.dims <- dim(sub.LINKAGES.output)
    
    sub.LINKAGES.pft <- subset(LINKAGES.pft, year == y)
    sub.LINKAGES.pft.dims <- dim(sub.LINKAGES.pft)
    
    DEFAULT.C <- 0.48  ## mass percent C of biomass
    PLOT.AREA <- 833 ## m^2
    toKG <- 1000 ## Kg in Mg
    yearSecs <- (3.15569 * 10^7) 
    Tconst <- .012
    
    ## Setup outputs for netCDF file in appropriate units
    output <- list()
    ## standard variables: Carbon Pools
    output[[1]] <- ((sub.LINKAGES.output$agBiomass / PLOT.AREA) / Tconst * DEFAULT.C) # Above Ground Biomass in kgC/m2
    output[[2]] <- (sub.LINKAGES.output$agBiomass / PLOT.AREA / Tconst * DEFAULT.C) # Total Live Biomass in kgC/m2 (no distinction from AGB in LINKAGES)
    output[[3]] <- (sub.LINKAGES.output$leafLitter + sub.LINKAGES.output$soilOM) / PLOT.AREA * DEFAULT.C # TotSoilCarb in kgC/m2
    output[[4]] <- c(sub.LINKAGES.output$agBiomass,sub.LINKAGES.output$soilOM) / PLOT.AREA / c(Tconst,1) * DEFAULT.C #Carb Pools in kgC/m2 #took out leaf litter because it's calculated as kg/tree in the model
    output[[5]] <- c("AGB","Soil Organic Matter") #poolname
    output[[6]] <- (sub.LINKAGES.output$agNPP / PLOT.AREA / yearSecs * DEFAULT.C * toKG) # GWBI = NPP in LINKAGES
    output[[7]] <- (sub.LINKAGES.output$soilResp / PLOT.AREA / yearSecs * toKG) # HeteroResp in kgC/m^2/s
    output[[8]] <- (sub.LINKAGES.output$agNPP / PLOT.AREA / yearSecs * DEFAULT.C * toKG) # NPP = GWBI in LINKAGES
    output[[9]] <- ((sub.LINKAGES.output$agNPP - sub.LINKAGES.output$soilResp) / PLOT.AREA / yearSecs * DEFAULT.C * toKG) # NEE #possibly questionable
    output[[10]] <- ((sub.LINKAGES.output$ET) / yearSecs) # Evap in kg/m^2/s
    
    output[[11]] <- (sub.LINKAGES.pft[2:10]/ PLOT.AREA * DEFAULT.C * toKG)
    
    if(sum(sub.LINKAGES.pft[2:length(sub.LINKAGES.pft)])==0){
      output[[12]] <- sub.LINKAGES.pft[2:10]
      } else {
        output[[12]] <- (sub.LINKAGES.pft[2:10]/ PLOT.AREA * DEFAULT.C * toKG) / sum((sub.LINKAGES.pft[2:length(sub.LINKAGES.pft)]/ PLOT.AREA * DEFAULT.C * toKG))
      }
      
    
      
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
                        vals = 1:2,
                        longname = "Carbon Pools") 
    dim.cpools1 <- ncdim_def("cpools", "",
                         vals = 1:2,
                         longname = "Carbon Pools", create_dimvar=FALSE)  
    dim.pfts <- ncdim_def("pfts", "",
                             vals = 1:9,
                             longname = "PFTs", create_dimvar=FALSE)  
        
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
    var[[10]]  <- ncvar_def("Evap", "kg/m2/s", list(dim.lat, dim.lon, dim.t), -999)
    
    var[[11]]  <- ncvar_def("AGB.pft", "kgC/m2",list(dim.pfts, dim.lat, dim.lon, dim.t),-999)
    var[[12]]  <- ncvar_def("Fcomp", "kgC/kgC",list(dim.pfts, dim.lat, dim.lon, dim.t),-999)
    
    #******************** Declar netCDF variables ********************#
    
   
    ### Output netCDF data
    nc <- nc_create(file.path(outdir, paste(sprintf("%04d",year_vec[y]),"nc", sep=".")), var)
    varfile <- file(file.path(outdir, paste(year_vec[y], "nc", "var", sep=".")), "w")
    for(i in 1:length(var)){
      #print(i)
      ncvar_put(nc,var[[i]],output[[i]])  
      cat(paste(var[[i]]$name, var[[i]]$longname), file=varfile, sep="\n")
    }
    close(varfile)
    nc_close(nc)
    
  } ### End of year loop
  
  
  
}

