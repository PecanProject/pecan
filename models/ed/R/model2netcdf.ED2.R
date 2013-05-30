#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#---------------------------------------------------------------------##' Modified from Code to convert ED2.1's HDF5 output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.ED2
##' @title Code to convert ED2's -T- HDF5 output into netCDF format
##'
##' @param outdir Location of ED model output
##' @export
##'
##' @author Michael Dietze, Shawn Serbin, Rob Kooper
## modified M. Dietze 07/08/12
## modified S. Serbin 05/06/13
model2netcdf.ED2 <- function(outdir) {
  flist <- dir(outdir,"-T-")
  if (length(flist) == 0) {
    print(paste("*** WARNING: No tower output for :",outdir))
    break
  }
  
  ## extract data info from file names?
  yr <- rep(NA,length(flist))
  for(i in 1:length(flist)){
    ## tmp <- sub(run.id,"",flist[i])  # Edited by SPS
    ## tmp <- sub("-T-","",tmp)        # Edited by SPS
    index <- gregexpr("-T-",flist[i])[[1]]
    index <- index[1]
    yr[i] <- as.numeric(substr(flist[i],index+3,index+6))
    ## yr[i] <- as.numeric(substr(tmp,1,4)) # Edited by SPS
  }
  
  ## TODO RK : need to shift data based on start date
  
  ## set up storage
  ##block <- 24 # assumes hourly  
  block <- 48 # assumes half-hourly  # Need to generalize (SPS)
  
  add <- function(dat, col, row) {
    values <- 366 * block
    dims <- dim(dat)
    ## simple check to see if dat is a single value
    if (is.null(dims)) {
      if (length(out) < col) {
        out[[col]] <- array(dat, dim=(values))
      } else {
        out[[col]] <- abind(out[[col]], array(dat, dim=(values)), along=1)
      }
    }
    
    ## copy data
    else {
      if (length(dims) == 2) {
        dat <- t(dat)
        dims <- dim(dat)
      } else if (length(dims) > 2) {
        logger.warn("Don't know how to handle larger arrays yet.")
      }
      dims[1] <- values - dims[1]
      if (length(out) < col) {
        out[[col]] <- abind(dat, array(-999, dim=dims), along=1)
      } else {
        out[[col]] <- abind(out[[col]], dat, array(-999, dim=dims), along=1)
      }
    }
    
    ## finally make sure we use -999 for invalid values
    out[[col]][is.null(out[[col]])] <- -999
    out[[col]][is.na(out[[col]])] <- -999
    
    return(out)
  }
  
  getHdf5Data <- function(nc, var) {
    if(var %in% names(nc$var)) {
      return(ncvar_get(nc, var))
    } else {
      logger.warn("Could not find", var, "in ed hdf5 output.")
      return(-999)
    }
  }
  
  # make sure only to convert those values that are not -999
  conversion <- function(col, mult) {
    out[[col]][out[[col]] != -999] <- out[[col]][out[[col]] != -999] * mult
    return(out)
  }
  
  checkTemp <- function(col) {
    out[[col]][out[[col]] == 0] <- -999
    return(out)
  }
  
  ## loop over files  ### break by YEAR
  yrs <- sort(unique(yr))
  for(y in 1:length(yrs)){
    ysel <- which(yr == yrs[y])
    n <- length(ysel)
    out <- list()
    ## prevTime <- NULL
    ## print(y)
    print(paste("----- Processing year: ",yrs[y]))
    ## if(haveTime) prevTime <- progressBar()
    row <- 1
    for(i in ysel){
      ncT <- nc_open(file.path(outdir, flist[i]))
      if (file.exists(file.path(outdir, sub('-T-', '-Y-', flist[i])))) {
        ncY <- nc_open(file.path(outdir, sub('-T-', '-Y-', flist[i])))
        slzdata <- getHdf5Data(ncY, 'SLZ')
        nc_close(ncY)
      } else {
        slzdata <- array(c(-2.00, -1.50, -1.00, -0.80, -0.60, -0.40, -0.20, -0.10, -0.05))
      }
      
      ## store for later use, will only use last data
      dz <- diff(slzdata)
      dz <- dz[dz != 0.0]
      
      ## out <- add(getHdf5Data(ncT, 'TOTAL_AGB,1,row) ## AbvGrndWood
      out <- add(getHdf5Data(ncT, 'AVG_BDEAD')*0.7,1,row) ## AbvGrndWood
      out <- add(getHdf5Data(ncT, 'AVG_PLANT_RESP'),2,row) ## AutoResp
      out <- add(-999,3,row) ## CarbPools
      out <- add(getHdf5Data(ncT, 'AVG_CO2CAN'),4,row) ## CO2CAS
      out <- add(-999,5,row) ## CropYield
      out <- add(getHdf5Data(ncT, 'AVG_GPP'),6,row) ## GPP
      out <- add(getHdf5Data(ncT, 'AVG_HTROPH_RESP'),7,row) ## HeteroResp
      out <- add(getHdf5Data(ncT, 'AVG_GPP') - getHdf5Data(ncT, 'AVG_PLANT_RESP') - getHdf5Data(ncT, 'AVG_HTROPH_RESP'),8,row) ## NEE
      out <- add(getHdf5Data(ncT, 'AVG_GPP') - getHdf5Data(ncT, 'AVG_PLANT_RESP'),9,row) ## NPP
      out <- add(getHdf5Data(ncT, 'AVG_HTROPH_RESP') + getHdf5Data(ncT, 'AVG_PLANT_RESP'),10,row) ## TotalResp
      ## out <- add(getHdf5Data(ncT, 'AVG_BDEAD + getHdf5Data(ncT, 'AVG_BALIVE,11,row) ## TotLivBiom
      out <- add(-999,11,row) ## TotLivBiom
      out <- add(getHdf5Data(ncT, 'AVG_FSC')+getHdf5Data(ncT, 'AVG_STSC')+getHdf5Data(ncT, 'AVG_SSC'),12,row) ## TotSoilCarb
      
      ## depth from surface to frozen layer
      tdepth <- 0
      fdepth <- 0
      soiltemp <- getHdf5Data(ncT, 'AVG_SOIL_TEMP')
      if(length(dim(soiltemp)) == 3){
        fdepth <- array(0,dim=dim(soiltemp)[1:2])
        tdepth <- array(0,dim=dim(soiltemp)[1:2])
        for(t in 1:dim(soiltemp)[1]){ #time
          for(p in 1:dim(soiltemp)[2]){ #polygon
            for(i in dim(soiltemp)[3]:2){ #depth
              if(fdepth[t,p] == 0 & soiltemp[t,p,i] < 273.15 & soiltemp[t, p, i-1] > 273.13){
                fdepth[t,p] <- i
              }
              if(tdepth[t,p] == 0 & soiltemp[t,p,i] > 273.15 & soiltemp[t, p, i-1] < 273.13){
                tdepth[t,p] <- i
              }
            }
            SLZ <- c(slzdata[t,],0.0)
            z1 <- (SLZ[fdepth[t,p]+1]+SLZ[fdepth[t,p]])/2
            z2 <- (SLZ[fdepth[t,p]]+SLZ[fdepth[t,p]-1])/2
            if(fdepth[t,p] > 0) {
              fdepth[t,p] <- z1 + (z2-z1) * (273.15 - soiltemp[t, p, fdepth[t, p]]) /
                (soiltemp[t, p, fdepth[t, p] - 1] - soiltemp[t, p, fdepth[t, p]])
            }
            if(tdepth[t,p] > 0) {
              SLZ <- c(slzdata[t,],0.0)
              z1 <- (SLZ[tdepth[t,p]+1]+SLZ[tdepth[t,p]])/2
              z2 <- (SLZ[tdepth[t,p]]+SLZ[tdepth[t,p]-1])/2
              tdepth[t,p] <- z1 + (z2-z1) * (273.15 - soiltemp[t, p, tdepth[t, p]]) /
                (soiltemp[t, p, tdepth[t, p] - 1] - soiltemp[t, p, tdepth[t, p]])
            }
          }
        }
      } else {
        # no polygons, just time vs depth?
        fdepth <- rep(0, ncol(soiltemp))
        tdepth <- rep(0, ncol(soiltemp))
        for(t in 1:ncol(soiltemp)){ #time
          for(d in 2:nrow(soiltemp)){ #depth
            if(fdepth[t] == 0 & soiltemp[d,t] < 273.15 & soiltemp[d-1,t] > 273.13){
              fdepth[t] <- d
            }
            if(tdepth[t] == 0 & soiltemp[d,t] > 273.15 & soiltemp[d-1,t] < 273.13){
              tdepth[t] <- d
            }
          }
          if(fdepth[t] > 0) {
            SLZ <- c(slzdata,0.0)
            z1 <- (SLZ[fdepth[t]+1]+SLZ[fdepth[t]])/2
            z2 <- (SLZ[fdepth[t]]+SLZ[fdepth[t]-1])/2
            fdepth[t] <- z1 + (z2-z1) * (273.15 - soiltemp[fdepth[t], t]) / (soiltemp[fdepth[t] - 1, t] - soiltemp[fdepth[t], t])
          }
          if(tdepth[t] > 0) {
            SLZ <- c(slzdata,0.0)
            z1 <- (SLZ[tdepth[t]+1]+SLZ[tdepth[t]])/2
            z2 <- (SLZ[tdepth[t]]+SLZ[tdepth[t]-1])/2
            tdepth[t] <- z1 + (z2-z1) * (273.15 - soiltemp[tdepth[t], t]) / (soiltemp[tdepth[t] - 1, t] - soiltemp[tdepth[t], t])
          }
        }
      }
      
      out <- add(fdepth,13,row) ## Fdepth
      out <- add(getHdf5Data(ncT, 'AVG_SNOWDEPTH'),14,row) ## SnowDepth
      out <- add(1-getHdf5Data(ncT, 'AVG_SNOWFRACLIQ'),15,row) ## SnowFrac
      out <- add(tdepth,16,row) ## Tdepth
      out <- add(getHdf5Data(ncT, 'AVG_ATM_CO2'),17,row) ## CO2air
      out <- add(getHdf5Data(ncT, 'AVG_RLONG'),18,row) ## Lwdown
      out <- add(getHdf5Data(ncT, 'AVG_PRSS'),19,row) ## Psurf
      out <- add(getHdf5Data(ncT, 'AVG_ATM_SHV'),20,row) ## Qair
      out <- add(getHdf5Data(ncT, 'AVG_PCPG'),21,row) ## Rainf
      ##out <- add(getHdf5Data(ncT, 'AVG_NIR_BEAM') +
      ##           getHdf5Data(ncT, 'AVG_NIR_DIFFUSE')+
      ##           getHdf5Data(ncT, 'AVG_PAR_BEAM')+
      ##           getHdf5Data(ncT, 'AVG_PAR_DIFFUSE'),22,row) ## Swdown
      out <- add(getHdf5Data(ncT, 'AVG_PAR_BEAM')+
                   getHdf5Data(ncT, 'AVG_PAR_DIFFUSE'),22,row) ## Swdown
      out <- add(getHdf5Data(ncT, 'AVG_ATM_TMP'),23,row) ## Tair
      out <- add(getHdf5Data(ncT, 'AVG_VELS'),24,row) ## Wind
      ##out <- add(getHdf5Data(ncT, 'AVG_RLONG')-getHdf5Data(ncT, 'AVG_RLONGUP'),25,row) ## Lwnet
      out <- add(-999,25,row) ## Lwnet
      ##out <- add(getHdf5Data(ncT, 'AVG_SENSIBLE_GC') + getHdf5Data(ncT, 'AVG_VAPOR_GC')*2272000,26,row) ## Qg
      out <- add(-999,26,row) ## Qg
      ##out <- add(getHdf5Data(ncT, 'AVG_SENSIBLE_TOT'),27,row) ## Qh
      out <- add(getHdf5Data(ncT, 'AVG_SENSIBLE_AC'),27,row) ## Qh
      out <- add(getHdf5Data(ncT, 'AVG_EVAP'),28,row) ## Qle
      out <- add(-999,29,row) ## Swnet
      out <- add(-999,30,row) ## RootMoist
      out <- add(getHdf5Data(ncT, 'AVG_TRANSP'),31,row) ## Tveg
      out <- add(getHdf5Data(ncT, 'ZBAR'),32,row) ## WaterTableD
      out <- add(-999,33,row) ## fPAR
      ##lai <- matrix(apply(getHdf5Data(ncT, 'LAI_PFT'),1,sum,na.rm=TRUE),nrow=block)
      ## out <- add(lai,34,row) ## LAI******************
      out <- add(-999,34,row) ## LAI
      ##z <- getHdf5Data(ncT, 'SLZ')
      ##if(z[length(z)] < 0.0) z <- c(z,0.0)
      ##dz <- diff(z)
      ##dz <- dz[dz != 0.0]
      ##fliq <- sum(getHdf5Data(ncT, 'AVG_SOIL_FRACLIQ')*dz)/-min(z)
      fliq <- NA#getHdf5Data(ncT, 'AVG_SOIL_FRACLIQ')
      out <- add(1-fliq,35,row) ## SMFrozFrac
      out <- add(fliq,36,row) ## SMLiqFrac
      out <- add(getHdf5Data(ncT, 'AVG_SOIL_WATER'),37,row) ## SoilWater  **********
      ##out <- add(sum(soiltemp*dz)/-min(z),38) ## SoilTemp
      out <- add(soiltemp,38,row) ## SoilTemp
      out <- add(soiltemp*NA,39,row) ## SoilWet
      out <- add(getHdf5Data(ncT, 'AVG_ALBEDO'),40,row) ## Albedo
      out <- add(getHdf5Data(ncT, 'AVG_SNOWTEMP'),41,row) ## SnowT
      out <- add(getHdf5Data(ncT, 'AVG_SNOWMASS'),42,row) ## SWE
      out <- add(getHdf5Data(ncT, 'AVG_VEG_TEMP'),43,row) ## VegT
      out <- add(getHdf5Data(ncT, 'AVG_EVAP')+getHdf5Data(ncT, 'AVG_TRANSP'),44,row) ## Evap
      out <- add(getHdf5Data(ncT, 'AVG_RUNOFF'),45,row) ## Qs
      out <- add(getHdf5Data(ncT, 'BASEFLOW'),46,row) ## Qsb
      
      nc_close(ncT)
      ## prevTime <- progressBar(i/n,prevTime)
      row <- row + block
    }  ## end file loop 
    
    #out[[10]] <- out[[10]]*1.2e-8  
    ## TODO see bug #1174
    ## for(t in 1:dim(out[[37]])[1]){
    ##  for(p in 1:dim(out[[37]])[2]){
    ##    out[[37]][t,p,] <- out[[37]][t,p,]*1000*dz ## m/m -> kg/m2
    ##  }
    ##}
    
    ## declare variables
    ## need to SHIFT for partial years **********************
    t <- ncdim_def(name = "time",
                   units = paste0("days since ",
                                  year(settings$run$start.date), "-01-01 00:00:00"),
                   vals = 1:dim(out[[1]])[1]) / block,
    calendar = "standard", unlim = TRUE)

zg <- ncdim_def("SoilLayerMidpoint","meters",slzdata[1:length(dz)]+dz/2)
lat <- ncdim_def("lat", "degrees_east",
                 vals =  as.numeric(settings$run$site$lat),
                 longname = "station_latitude") 
lon <- ncdim_def("lon", "degrees_north",
                 vals = as.numeric(settings$run$site$lon),
                 longname = "station_longitude")

var <- list()
out <- conversion( 1, 0.1)     ## tC/ha     -> kg/m2
var[[1]]  <- ncvar_def("AbvGrndWood","kg m-2", list(lat, lon, t),-999)
out <- conversion( 2, 1.2e-8)  ## umol/m2 s-1 -> kg/m2 s-1
var[[2]]  <- ncvar_def("AutoResp","kg m-2 s-1", list(lat, lon, t),-999)  # Edited by SPS.  Changed from kg/m2 s-12 to kg/m2 s-1
var[[3]]  <- ncvar_def("CarbPools","kg m-2", list(lat, lon, t),-999)
var[[4]]  <- ncvar_def("CO2CAS","ppmv", list(lat, lon, t),-999)
var[[5]]  <- ncvar_def("CropYield","kg m-2", list(lat, lon, t),-999)
out <- conversion( 6, 1.2e-8)  ## umol/m2 s-1 -> kg m-2 s-1
var[[6]]  <- ncvar_def("GPP","kg m-2 s-12", list(lat, lon, t),-999)     # Edited by SPS.  Changed from kg m-2 s-12 to kg m-2 s-1
out <- conversion( 7, 1.2e-8)  ## umol/m2 s-1 -> kg m-2 s-1
var[[7]]  <- ncvar_def("HeteroResp","kg m-2 s-1", list(lat, lon, t),-999) # Edited by SPS.  Changed from kg m-2 s-12 to kg m-2 s-1
out <- conversion( 8, 1.2e-8)  ## umol/m2 s-1 -> kg m-2 s-1
var[[8]]  <- ncvar_def("NEE","kg m-2 s-1", list(lat, lon, t),-999)     # Edited by SPS.  Changed from kg m-2 s-12 to kg m-2 s-1
out <- conversion( 9, 1.2e-8)  ## umol/m2 s-1 -> kg m-2 s-1
var[[9]]  <- ncvar_def("NPP","kg m-2 s-1", list(lat, lon, t),-999)     # Edited by SPS.  Changed from kg m-2 s-12 to kg m-2 s-1
out <- conversion(10, 1.2e-8)  ## umol/m2 s-1 -> kg m-2 s-1
var[[10]] <- ncvar_def("TotalResp","kg m-2 s-1", list(lat, lon, t),-999) # Edited by SPS.  Changed from kg m-2 s-12 to kg m-2 s-1
var[[11]] <- ncvar_def("TotLivBiom","kg m-2", list(lat, lon, t),-999) # Edited by SPS.  Changed from kg m-2 s-2 to kg m-2
var[[12]] <- ncvar_def("TotSoilCarb","kg m-2", list(lat, lon, t),-999)
var[[13]] <- ncvar_def("Fdepth","m", list(lat, lon, t),-999)
var[[14]] <- ncvar_def("SnowDepth","m", list(lat, lon, t),-999)
var[[15]] <- ncvar_def("SnowFrac","-", list(lat, lon, t),-999)
var[[16]] <- ncvar_def("Tdepth","m", list(lat, lon, t),-999)
var[[17]] <- ncvar_def("CO2air","ppmv", list(lat, lon, t),-999)
var[[18]] <- ncvar_def("Lwdown","W m-2", list(lat, lon, t),-999)
var[[19]] <- ncvar_def("Psurf","Pa", list(lat, lon, t),-999)
var[[20]] <- ncvar_def("Qair","kg kg-1", list(lat, lon, t),-999)
var[[21]] <- ncvar_def("Rainf","kg m-2 s-1", list(lat, lon, t),-999)
var[[22]] <- ncvar_def("Swdown","W m-2", list(lat, lon, t),-999)
out <- checkTemp(23)
var[[23]] <- ncvar_def("Tair","K", list(lat, lon, t),-999)
var[[24]] <- ncvar_def("Wind","W m-2", list(lat, lon, t),-999)
var[[25]] <- ncvar_def("Lwnet","W m-2", list(lat, lon, t),-999)
var[[26]] <- ncvar_def("Qg","W m-2", list(lat, lon, t),-999)
var[[27]] <- ncvar_def("Qh","W m-2", list(lat, lon, t),-999)
var[[28]] <- ncvar_def("Qle","W m-2", list(lat, lon, t),-999)
var[[29]] <- ncvar_def("Swnet","W m-2", list(lat, lon, t),-999)
var[[30]] <- ncvar_def("RootMoist","kg m-2", list(lat, lon, t),-999)
var[[31]] <- ncvar_def("Tveg","kg m-2 s-1", list(lat, lon, t),-999)
var[[32]] <- ncvar_def("WaterTableD","m", list(lat, lon, t),-999)
var[[33]] <- ncvar_def("fPAR","-", list(lat, lon, t),-999)
var[[34]] <- ncvar_def("LAI","m2 m-2", list(lat, lon, t),-999)
##var[[35]] <- ncvar_def("SMFrozFrac","-",list(t,zg),-999)
##var[[36]] <- ncvar_def("SMLiqFrac","-",list(t,zg),-999)
var[[35]] <- ncvar_def("SMFrozFrac","-",list(t),-999)
var[[36]] <- ncvar_def("SMLiqFrac","-",list(t),-999)
var[[37]] <- ncvar_def("SoilMoist","kg m-2",list(t,zg),-999)
out <- checkTemp(38)
var[[38]] <- ncvar_def("SoilTemp","K",list(t,zg),-999)
var[[39]] <- ncvar_def("SoilWet","-",list(t,zg),-999)
var[[40]] <- ncvar_def("Albedo","-", list(lat, lon, t),-999)
out <- checkTemp(41)
var[[41]] <- ncvar_def("SnowT","K", list(lat, lon, t),-999)
var[[42]] <- ncvar_def("SWE","kg m-2", list(lat, lon, t),-999)
out <- checkTemp(43)
var[[43]] <- ncvar_def("VegT","K", list(lat, lon, t),-999)
var[[44]] <- ncvar_def("Evap","kg m-2 s-1", list(lat, lon, t),-999)
var[[45]] <- ncvar_def("Qs","kg m-2 s-1", list(lat, lon, t),-999)
var[[46]] <- ncvar_def("Qsb","kg m-2 s-1", list(lat, lon, t),-999)


## write ALMA
nc <- nc_create(file.path(outdir, paste(yrs[y], "nc", sep=".")), var)
for(i in 1:length(var)){
  ## TODO see bug #1174
  if (i != 37 && i != 38 && i != 39) {
    ncvar_put(nc,var[[i]],out[[i]])  
  }
}
nc_close(nc)
closeAllConnections()
  }  ## end year loop

closeAllConnections() 

}  ## end model2netcdf.ED2
#==================================================================================================#

####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
