#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------#
##' Modified from Code to convert ED2.1's HDF5 output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.ED2
##' @title Code to convert ED2's -T- HDF5 output into netCDF format
##'
##' @param outdir Location of ED model output
##' @export
##'
##' @author Michael Dietze
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

  ## set up storage
  ##block <- 24 # assumes hourly
  block <- 48 # assumes half-hourly  # Needs to be generalized (SPS)
  
  add <- function(dat, col, row){
    
    ## first clone and fill data
    if(length(dat) < block) dat <- rep(dat,block)[1:block] 
    dat[is.null(dat)] <- -999
    dat[is.na(dat)] <- -999
    
    dims <- dim(dat)
    if(block == 1) dims = c(1,dim(dat))
    
    ## Option 1 - add data anew
    if(length(out) < col){
      bdim <- dims
      bdim[1] <- 366*block
      ##    if(length(dat) > 1){
      ##      out[[col]] <- matrix(NA,366*block,length(dat))
      ##    } else {
      ##      out[[col]] <- rep(NA,366*block)
      ##    }
      ##array(NA,dim=dims) 
      out[[col]] <- array(NA,dim=bdim)
    }
    ndim <-length(dim(dat))
    if(ndim == 1){  #block must be 1
      out[[col]][row] <- dat
    } else if(ndim == 2){
      for(i in 1:dims[1]){
        out[[col]][row+i-1,] <- dat[i,]
      }
    } else if (ndim ==3){
      for(i in 1:dims[1]){
        for(j in 1:dims[2]){
          out[[col]][row+i-1,j,] <- dat[i,j,]
        }
      }
    }
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
      dat <- hdf5load(file.path(outdir, flist[i]), load=FALSE)
      
      ## out <- add(dat$TOTAL_AGB,1,row) ## AbvGrndWood
      out <- add(dat$AVG_BDEAD*0.7,1,row) ## AbvGrndWood
      out <- add(dat$AVG_PLANT_RESP,2,row) ## AutoResp
      out <- add(-999,3,row) ## CarbPools
      out <- add(dat$AVG_CO2CAN,4,row) ## CO2CAS
      out <- add(-999,5,row) ## CropYield
      out <- add(dat$AVG_GPP,6,row) ## GPP
      out <- add(dat$AVG_HTROPH_RESP,7,row) ## HeteroResp
      out <- add(dat$AVG_GPP - dat$AVG_PLANT_RESP - dat$AVG_HTROPH_RESP,8,row) ## NEE
      out <- add(dat$AVG_GPP - dat$AVG_PLANT_RESP,9,row) ## NPP
      out <- add(dat$AVG_HTROPH_RESP + dat$AVG_PLANT_RESP,10,row) ## TotalResp
      ## out <- add(dat$AVG_BDEAD + dat$AVG_BALIVE,11,row) ## TotLivBiom
      out <- add(-999,11,row) ## TotLivBiom
      out <- add(dat$AVG_FSC+dat$AVG_STSC+dat$AVG_SSC,12,row) ## TotSoilCarb
      ## depth from surface to frozen layer
      fdepth <- 0
      if(length(dim(dat$AVG_SOIL_TEMP)) == 3){
        fdepth <- array(0,dim=dim(dat$AVG_SOIL_TEMP)[1:2])
        for(t in 1:dim(dat$AVG_SOIL_TEMP)[1]){ #time
          for(p in 1:dim(dat$AVG_SOIL_TEMP)[2]){ #polygon
            for(i in dim(dat$AVG_SOIL_TEMP)[3]:2){ #depth
              if(dat$AVG_SOIL_TEMP[t,p,i] < 273.15 &
                 dat$AVG_SOIL_TEMP[t, p, i-1] > 273.13){
                fdepth[t,p] <- i
                break
              }
            }
            if(fdepth[t,p] > 0) {
              SLZ <- c(dat$SLZ[t,],0.0)
              z1 <- (SLZ[fdepth[t,p]+1]+SLZ[fdepth[t,p]])/2
              z2 <- (SLZ[fdepth[t,p]]+SLZ[fdepth[t,p]-1])/2
              fdepth[t,p] <- z1 + (z2-z1) *
                (273.15 - dat$AVG_SOIL_TEMP[t, p, fdepth[t, p]]) /
                  (dat$AVG_SOIL_TEMP[t, p, fdepth[t, p] - 1] -
                   dat$AVG_SOIL_TEMP[t, p, fdepth[t, p]])
            }
          }
        }
      } else {
        fdepth <- rep(0, nrow(dat$AVG_SOIL_TEMP))
        for(p in 1:dim(dat$AVG_SOIL_TEMP)[1]){ #polygon
          for(i in dim(dat$AVG_SOIL_TEMP)[2]:2){ #depth
            if(dat$AVG_SOIL_TEMP[p,i] < 273.15 & dat$AVG_SOIL_TEMP[p,i-1]>273.13){
              fdepth[p] <- i
              break
            }
          }
          if(fdepth[p] > 0) {
            SLZ <- c(dat$SLZ,0.0)
            z1 <- (SLZ[fdepth[p]+1]+SLZ[fdepth[p]])/2
            z2 <- (SLZ[fdepth[p]]+SLZ[fdepth[p]-1])/2
            fdepth[p] <- z1 + (z2-z1) *
              (273.15 - dat$AVG_SOIL_TEMP[p, fdepth[p]]) /
                (dat$AVG_SOIL_TEMP[p, fdepth[p] - 1] -
                 dat$AVG_SOIL_TEMP[p, fdepth[p]])
          }
        }
      }

      out <- add(fdepth,13,row) ## Fdepth
      out <- add(dat$AVG_SNOWDEPTH,14,row) ## SnowDepth
      out <- add(1-dat$AVG_SNOWFRACLIQ,15,row) ## SnowFrac
      fdepth <- 0
      if(length(dim(dat$AVG_SOIL_TEMP)) == 3){
        fdepth <- array(0,dim=dim(dat$AVG_SOIL_TEMP)[1:2])
        for(t in 1:dim(dat$AVG_SOIL_TEMP)[1]){ #time
          for(p in 1:dim(dat$AVG_SOIL_TEMP)[2]){ #polygon
            for(i in dim(dat$AVG_SOIL_TEMP)[3]:2){ #depth
              if(dat$AVG_SOIL_TEMP[t,p,i] > 273.15 &
                 dat$AVG_SOIL_TEMP[t, p, i-1] < 273.13){
                fdepth[t,p] <- i
                break
              }
            }
            if(fdepth[t,p] > 0) {
              SLZ <- c(dat$SLZ,0.0)
              z1 <- (SLZ[fdepth[t,p]+1]+SLZ[fdepth[t,p]])/2
              z2 <- (SLZ[fdepth[t,p]]+SLZ[fdepth[t,p]-1])/2
              fdepth[t,p] <- z1 + (z2-z1) *
                (273.15 - dat$AVG_SOIL_TEMP[t, p, fdepth[t, p]]) /
                  (dat$AVG_SOIL_TEMP[t, p, fdepth[t, p] - 1] -
                   dat$AVG_SOIL_TEMP[t, p, fdepth[t, p]])
            }
          }
        }
      } else {
        for(p in 1:dim(dat$AVG_SOIL_TEMP)[1]){ #polygon
          for(i in dim(dat$AVG_SOIL_TEMP)[2]:2){ #depth
            if(dat$AVG_SOIL_TEMP[p, i] > 273.15 &
               dat$AVG_SOIL_TEMP[p, i-1] < 273.13){
              fdepth[p] <- i
              break
            }
          }
          if(fdepth[p] > 0) {
            SLZ <- c(dat$SLZ,0.0)
            z1 <- (SLZ[fdepth[p]+1]+SLZ[fdepth[p]])/2
            z2 <- (SLZ[fdepth[p]]+SLZ[fdepth[p]-1])/2
            fdepth[p] <- z1 + (z2-z1) * (273.15 - dat$AVG_SOIL_TEMP[p, fdepth]) /
              (dat$AVG_SOIL_TEMP[p, fdepth - 1] - dat$AVG_SOIL_TEMP[p, fdepth])
          }
        }
      }
      out <- add(fdepth,16,row) ## Tdepth
      out <- add(dat$AVG_ATM_CO2,17,row) ## CO2air
      out <- add(dat$AVG_RLONG,18,row) ## Lwdown
      out <- add(dat$AVG_PRSS,19,row) ## Psurf
      out <- add(dat$AVG_ATM_SHV,20,row) ## Qair
      out <- add(dat$AVG_PCPG,21,row) ## Rainf
      ##out <- add(dat$AVG_NIR_BEAM +
      ##           dat$AVG_NIR_DIFFUSE+
      ##           dat$AVG_PAR_BEAM+
      ##           dat$AVG_PAR_DIFFUSE,22,row) ## Swdown
      out <- add(dat$AVG_PAR_BEAM+
                 dat$AVG_PAR_DIFFUSE,22,row) ## Swdown
      out <- add(dat$AVG_ATM_TMP,23,row) ## Tair
      out <- add(dat$AVG_VELS,24,row) ## Wind
      ##out <- add(dat$AVG_RLONG-dat$AVG_RLONGUP,25,row) ## Lwnet
      out <- add(-999,25,row) ## Lwnet
      ##out <- add(dat$AVG_SENSIBLE_GC + dat$AVG_VAPOR_GC*2272000,26,row) ## Qg
      out <- add(-999,26,row) ## Qg
      ##out <- add(dat$AVG_SENSIBLE_TOT,27,row) ## Qh
      out <- add(dat$AVG_SENSIBLE_AC,27,row) ## Qh
      out <- add(dat$AVG_EVAP,28,row) ## Qle
      out <- add(-999,29,row) ## Swnet
      out <- add(-999,30,row) ## RootMoist
      out <- add(dat$AVG_TRANSP,31,row) ## Tveg
      out <- add(dat$ZBAR,32,row) ## WaterTableD
      out <- add(-999,33,row) ## fPAR
      ##lai <- matrix(apply(dat$LAI_PFT,1,sum,na.rm=TRUE),nrow=block)
      ## out <- add(lai,34,row) ## LAI******************
      out <- add(-999,34,row) ## LAI
      ##z <- dat$SLZ
      ##if(z[length(z)] < 0.0) z <- c(z,0.0)
      ##dz <- diff(z)
      ##dz <- dz[dz != 0.0]
      ##fliq <- sum(dat$AVG_SOIL_FRACLIQ*dz)/-min(z)
      fliq <- NA#dat$AVG_SOIL_FRACLIQ
      out <- add(1-fliq,35,row) ## SMFrozFrac
      out <- add(fliq,36,row) ## SMLiqFrac
      out <- add(dat$AVG_SOIL_WATER,37,row) ## SoilWater  **********
      ##out <- add(sum(dat$AVG_SOIL_TEMP*dz)/-min(z),38) ## SoilTemp
      out <- add(dat$AVG_SOIL_TEMP,38,row) ## SoilTemp
      out <- add(dat$AVG_SOIL_TEMP*NA,39,row) ## SoilWet
      out <- add(dat$AVG_ALBEDO,40,row) ## Albedo
      out <- add(dat$AVG_SNOWTEMP,41,row) ## SnowT
      out <- add(dat$AVG_SNOWMASS,42,row) ## SWE
      out <- add(dat$AVG_VEG_TEMP,43,row) ## VegT
      out <- add(dat$AVG_EVAP+dat$AVG_TRANSP,44,row) ## Evap
      out <- add(dat$AVG_RUNOFF,45,row) ## Qs
      out <- add(dat$BASEFLOW,46,row) ## Qsb
      
      ## prevTime <- progressBar(i/n,prevTime)
      row <- row + block
    }  ## end file loop

    ## process for output (unit conversions)
    z <- c(-9.00, -6.00, -4.00, -2.00, -1.40, -0.80, -0.54, -0.41, -0.32, -0.19, -0.13, -0.08) #dat$SLZ
    if(z[length(z)] < 0.0) z <- c(z,0.0)
    dz <- diff(z)
    dz <- dz[dz != 0.0]
  
    out[[1]]  <- out[[1]]*0.1      ## tC/ha     -> kg/m2
    out[[2]]  <- out[[2]]*1.2e-8   ## umol/m2/s -> kg/m2/s
    out[[6]]  <- out[[6]]*1.2e-8   ## umol/m2/s -> kg/m2/s
    out[[7]]  <- out[[7]]*1.2e-8   ## umol/m2/s -> kg/m2/s
    out[[8]]  <- out[[8]]*1.2e-8   ## umol/m2/s -> kg/m2/s
    out[[9]]  <- out[[9]]*1.2e-8   ## umol/m2/s -> kg/m2/s
    out[[10]] <- out[[10]]*1.2e-8  ## umol/m2/s -> kg/m2/s
    ## TODO see bug #1174
    ## for(t in 1:dim(out[[37]])[1]){
    ##  for(p in 1:dim(out[[37]])[2]){
    ##    out[[37]][t,p,] <- out[[37]][t,p,]*1000*dz ## m/m -> kg/m2
    ##  }
    ##}
  
    ## declare variables
    ## need to SHIFT for partial years **********************
    t <- dim.def.ncdf("time","seconds",(1:dim(out[[1]])[1])*3600.0*24.0/block)
    zg <- dim.def.ncdf("SoilLayerMidpoint","meters",z[1:length(dz)]+dz/2)
    
    var <- list()
    var[[1]]  <- var.def.ncdf("AbvGrndWood","kg/m2",t,-999)
    var[[2]]  <- var.def.ncdf("AutoResp","kg/m2/s",t,-999) # edited from kg/m2/s2 (SPS)
    var[[3]]  <- var.def.ncdf("CarbPools","kg/m2",t,-999)
    var[[4]]  <- var.def.ncdf("CO2CAS","ppmv",t,-999)
    var[[5]]  <- var.def.ncdf("CropYield","kg/m2",t,-999)
    var[[6]]  <- var.def.ncdf("GPP","kg/m2/s",t,-999) # edited from kg/m2/s2 (SPS)
    var[[7]]  <- var.def.ncdf("HeteroResp","kg/m2/s",t,-999) # edited from kg/m2/s2 (SPS)
    var[[8]]  <- var.def.ncdf("NEE","kg/m2/s",t,-999) # edited from kg/m2/s2 (SPS)
    var[[9]]  <- var.def.ncdf("NPP","kg/m2/s",t,-999) # edited from kg/m2/s2 (SPS)
    var[[10]] <- var.def.ncdf("TotalResp","kg/m2/s",t,-999)
    var[[11]] <- var.def.ncdf("TotLivBiom","kg/m2",t,-999) # edited from kg/m2/s2 (SPS)
    var[[12]] <- var.def.ncdf("TotSoilCarb","kg/m2",t,-999)
    var[[13]] <- var.def.ncdf("Fdepth","m",t,-999)
    var[[14]] <- var.def.ncdf("SnowDepth","m",t,-999)
    var[[15]] <- var.def.ncdf("SnowFrac","-",t,-999)
    var[[16]] <- var.def.ncdf("Tdepth","m",t,-999)
    var[[17]] <- var.def.ncdf("CO2air","ppmv",t,-999)
    var[[18]] <- var.def.ncdf("Lwdown","W/m2",t,-999)
    var[[19]] <- var.def.ncdf("Psurf","Pa",t,-999)
    var[[20]] <- var.def.ncdf("Qair","kg/kg",t,-999)
    var[[21]] <- var.def.ncdf("Rainf","kg/m2s",t,-999)
    var[[22]] <- var.def.ncdf("Swdown","W/m2",t,-999)
    var[[23]] <- var.def.ncdf("Tair","W/m2",t,-999)
    var[[24]] <- var.def.ncdf("Wind","W/m2",t,-999)
    var[[25]] <- var.def.ncdf("Lwnet","W/m2",t,-999)
    var[[26]] <- var.def.ncdf("Qg","W/m2",t,-999)
    var[[27]] <- var.def.ncdf("Qh","W/m2",t,-999)
    var[[28]] <- var.def.ncdf("Qle","W/m2",t,-999)
    var[[29]] <- var.def.ncdf("Swnet","W/m2",t,-999)
    var[[30]] <- var.def.ncdf("RootMoist","kg/m2",t,-999)
    var[[31]] <- var.def.ncdf("Tveg","kg/m2s",t,-999)
    var[[32]] <- var.def.ncdf("WaterTableD","m",t,-999)
    var[[33]] <- var.def.ncdf("fPAR","-",t,-999)
    var[[34]] <- var.def.ncdf("LAI","m2/m2",t,-999)
    ##var[[35]] <- var.def.ncdf("SMFrozFrac","-",list(t,zg),-999)
    ##var[[36]] <- var.def.ncdf("SMLiqFrac","-",list(t,zg),-999)
    var[[35]] <- var.def.ncdf("SMFrozFrac","-",list(t),-999)
    var[[36]] <- var.def.ncdf("SMLiqFrac","-",list(t),-999)
    var[[37]] <- var.def.ncdf("SoilMoist","kg/m2",list(t,zg),-999)
    var[[38]] <- var.def.ncdf("SoilTemp","K",list(t,zg),-999)
    var[[39]] <- var.def.ncdf("SoilWet","-",list(t,zg),-999)
    var[[40]] <- var.def.ncdf("Albedo","-",t,-999)
    var[[41]] <- var.def.ncdf("SnowT","K",t,-999)
    var[[42]] <- var.def.ncdf("SWE","kg/m2",t,-999)
    var[[43]] <- var.def.ncdf("VegT","K",t,-999)
    var[[44]] <- var.def.ncdf("Evap","kg/m2s",t,-999)
    var[[45]] <- var.def.ncdf("Qs","kg/m2s",t,-999)
    var[[46]] <- var.def.ncdf("Qsb","kg/m2s",t,-999)


    ## write ALMA
    nc <- create.ncdf(file.path(outdir, paste(yrs[y], "nc", sep=".")), var)
    for(i in 1:length(var)){
      ## TODO see bug #1174
      if (i != 37 && i != 38 && i != 39) {
        put.var.ncdf(nc,var[[i]],out[[i]])  
      }
    }
    close.ncdf(nc)
    closeAllConnections()
  }  ## end year loop
  
  closeAllConnections() 
  
}  ## end model2netcdf.ED2
#==================================================================================================#


####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
