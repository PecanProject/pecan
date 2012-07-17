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
##' @name model2netcdf.SIPNET
##' @title Function to convert SIPNET model output to standard netCDF format
##' @param outdir Location of SIPNET model output
##' @param run.id Name of SIPNET model output file.
##' 
##' @export
##' @author Shawn Serbin, Michael Dietze
model2netcdf.SIPNET <- function(outdir,run.id) {
  
  require(ncdf)
  
  ### Read in model output in SIPNET format
  sipnet.output <- read.table(paste(outdir,"/",run.id,"/",run.id,".out",sep=""),header=T,skip=1,sep='')
  sipnet.output.dims <- dim(sipnet.output)
  
  ### Determine number of years and output timestep
  num.years <- length(unique(sipnet.output$year))
  years <- unique(sipnet.output$year)
  timestep.s <- 86400/length(which(sipnet.output$year==years[1] & sipnet.output$day==1))
  out.day <- length(which(sipnet.output$year==years[1] & sipnet.output$day==1))
  
  ### Loop over years in SIPNET output to create separate netCDF outputs
  for (y in years){
    print(paste("---- Processing year: ",y))  # turn on for debugging
    
    ### Subset data for processing
    sub.sipnet.output <- subset(sipnet.output,year==y)
    sub.sipnet.output.dims <- dim(sub.sipnet.output)
    dayfrac = 1/out.day
    step <- seq(0,0.99,1/out.day)

    ### Setup outputs for netCDF file in appropriate units
    output <- list()
    output[[1]] <- sub.sipnet.output$year                       # Year
    output[[2]] <- sub.sipnet.output$day+step                   # Fractional day
    output[[3]] <- (sub.sipnet.output$gpp*0.001)/timestep.s     # GPP in kgC/m2/s
    #output[[4]] <- (sub.sipnet.output$npp*0.001)/timestep.s     # NPP in kgC/m2/s. Internal SIPNET calculation
    output[[4]] <- (sub.sipnet.output$gpp*0.001)/timestep.s -
      ((sub.sipnet.output$rAboveground*0.001)/timestep.s +
      (sub.sipnet.output$rRoot*0.001)/timestep.s)               # NPP in kgC/m2/s. Post SIPNET calculation
    output[[5]] <- (sub.sipnet.output$rtot*0.001)/timestep.s    # Total Respiration in kgC/m2/s
    output[[6]] <- (sub.sipnet.output$rAboveground*0.001)/timestep.s +
      (sub.sipnet.output$rRoot*0.001)/timestep.s                # Autotrophic Respiration in kgC/m2/s
    output[[7]] <- (sub.sipnet.output$rSoil*0.001)/timestep.s   # Heterotropic Respiration in kgC/m2/s
    output[[8]] <- (sub.sipnet.output$nee*0.001)/timestep.s     # NEE in kgC/m2/s
    #output[[9]] <- rep(-999,sipnet.output.dims[1])             # CarbPools
    output[[9]] <- (sub.sipnet.output$plantWoodC*0.001)         # Above ground wood kgC/m2
    output[[10]] <- (sub.sipnet.output$plantLeafC*0.001)        # Leaf C kgC/m2
    output[[11]] <- (sub.sipnet.output$plantWoodC*0.001)+
      (sub.sipnet.output$plantLeafC*0.001)+
      (sub.sipnet.output$coarseRootC*0.001)+
      (sub.sipnet.output$fineRootC*0.001)                       # Total living C kgC/m2
    output[[12]] <- (sub.sipnet.output$soil*0.001)+
      (sub.sipnet.output$litter*0.001)                          # Total soil C kgC/m2
    output[[13]] <- (sub.sipnet.output$fluxestranspiration*0.001)/timestep.s  #Transpiration kgW/m2/s
    output[[14]] <- (sub.sipnet.output$soilWater*10)            # Soil moisture kgW/m2
    output[[15]] <- (sub.sipnet.output$soilWetnessFrac)         # Fractional soil wetness
    output[[16]] <- (sub.sipnet.output$snow*10)                 # SWE
      
    #******************** Declar netCDF variables ********************#
    t <- dim.def.ncdf("time",paste("seconds since ",y,"-","01","-","01 ","00:00:00 -6:00",sep=""),
                      (1:sub.sipnet.output.dims[1]*timestep.s)) #cumulative time
    ### ***** Need to dynamically update the UTC offset here *****
    
    var <- list()
    var[[1]]  <- var.def.ncdf("Year","YYYY",t,-999,"Year of model output")                # year
    var[[2]]  <- var.def.ncdf("FracJulianDay","",t,-999,"Fractional Julian Date")         # Fractional Julian Day
    var[[3]]  <- var.def.ncdf("GPP","kgC/m2/s",t,-999,"Gross Primary Productivity")       # GPP in kgC/m2/s
    var[[4]]  <- var.def.ncdf("NPP","kgC/m2/s",t,-999,"Net Primary Productivity")         # NPP in kgC/m2/s
    var[[5]]  <- var.def.ncdf("TotalResp","kgC/m2/s",t,-999,"Total Respiration")          # Total Respiration in kgC/m2/s
    var[[6]]  <- var.def.ncdf("AutoResp","kgC/m2/s",t,-999,"Autotrophic Respiration")     # Autotrophic respiration in kgC/m2/s
    var[[7]]  <- var.def.ncdf("HeteroResp","kgC/m2/s",t,-999,"Heterotrophic Respiration") # Heterotrophic respiration in kgC/m2/s
    var[[8]]  <- var.def.ncdf("NEE","kgC/m2/s",t,-999,"Net Ecosystem Exchange")           # NEE in kgC/m2/s
    #var[[9]]  <- var.def.ncdf("CarbPools","kgC/m2/s",t,-999,"Size of each carbon pool")   # Carbon Pools
    var[[9]]  <- var.def.ncdf("AbvGrndWood","kgC/m2",t,-999,"Above ground woody biomass") # Above ground woody biomass KgC/m2
    var[[10]]  <- var.def.ncdf("LeafC","kgC/m2",t,-999,"Leaf carbon")                     # Leaf carbon KgC/m2
    var[[11]]  <- var.def.ncdf("TotLivBiom","kgC/m2",t,-999,"Total living biomass")       # Total living C kgC/m2
    var[[12]]  <- var.def.ncdf("TotSoilCarb","kgC/m2",t,-999,"Total soil carbon")         # Total soil C kgC/m2
    var[[13]]  <- var.def.ncdf("TVeg","kgC/m2/s",t,-999,"Transpiration")                  # Transpiration
    var[[14]]  <- var.def.ncdf("SoilMoist","kg/m2",t,-999,"Average Soil Moisture")        # Soil Moisture
    var[[15]]  <- var.def.ncdf("SoilMoistFrac","-",t,-999,"Average Fraction of Saturation") # Fractional soil wetness
    var[[16]]  <- var.def.ncdf("SWE","kg/m2",t,-999,"Snow Water Equivalent")              # SWE
    
    #******************** Declar netCDF variables ********************#
    nc <- create.ncdf(paste(outdir,"/",run.id,"/",run.id,".",y,".nc",sep=""),var)
    
    ### Output netCDF data
    for(i in 1:length(var)){
      #print(i)
      put.var.ncdf(nc,var[[i]],output[[i]])  
    }
    close.ncdf(nc)
    
  } ### End of year loop

} ### End of function
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
