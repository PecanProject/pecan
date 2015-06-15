## -------------------------------------------------------------------------------
## Copyright (c) 2012 University of Illinois, NCSA.
## All rights reserved. This program and the accompanying materials
## are made available under the terms of the 
## University of Illinois/NCSA Open Source License
## which accompanies this distribution, and is available at
## http://opensource.ncsa.illinois.edu/license.html
## ---------------------------------------------------------------------
##' Modified from Code to convert ED2.1's HDF5 output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.ED2
##' @title Code to convert ED2's -T- HDF5 output into netCDF format
##'
##' @param outdir Location of ED model output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##'
##' @author Michael Dietze, Shawn Serbin, Rob Kooper, Toni Viskari
## modified M. Dietze 07/08/12
## modified S. Serbin 05/06/13
model2netcdf.ED2 <- function(outdir, sitelat, sitelon, start_date, end_date) {
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
  block <- 48 # assumes half-hourly  # Need to generalize (SPS)
 
  add <- function(dat, col, row, year) {
    ## data is always given for whole year, except it will start always at 0
    ## the left over data is filled with 0's
    if (year == strftime(start_date, "%Y")) {
      start <- (as.numeric(strftime(start_date, "%j")) - 1) * block
    } else {
      start <- 0
    }
    if (year == strftime(end_date, "%Y")) {
      end <- as.numeric(strftime(end_date, "%j")) * block
    } else {
      end <- as.numeric(strftime(paste0(year, "-12-31"), "%j")) * block
    }

    dims <- dim(dat)
    if (is.null(dims)) {
      if (length(dat) == 1) {
        if (length(out) < col) {
          out[[col]] <- array(dat, dim=(end-start))
        } else {
          if (start != 0) {
            logger.warn("start date is not 0 this year, but data already exists in this col", col, "how is this possible?")
          }
          out[[col]] <- abind(out[[col]], array(dat, dim=(end-start)), along=1)
        }
      } else {
        logger.warn("expected a single value")
      }
    } else if (length(dims) == 1) {
      dat <- dat[1:(end-start)]
      if (length(out) < col) {
        out[[col]] <- dat
      } else {
        if (start != 0) {
          logger.warn("start date is not 0 this year, but data already exists in this col", col, "how is this possible?")
        }
        out[[col]] <- abind(out[[col]], dat, along=1)
      }
    } else if (length(dims) == 2) {
      dat <- t(dat)
      dims <- dim(dat)
      dat <- dat[1:(end-start),]
      if (length(out) < col) {
        out[[col]] <- dat
      } else {
        if (start != 0) {
          logger.warn("start date is not 0 this year, but data already exists in this col", col, "how is this possible?")
        }
        out[[col]] <- abind(out[[col]], dat, along=1)
      }
    } else {
      logger.debug("-------------------------------------------------------------")
      logger.debug("col=", col)
      logger.debug("length=", length(dat))
      logger.debug("start=", start)
      logger.debug("end=", end)
      logger.debug("dims=", dims)
      logger.warn("Don't know how to handle larger arrays yet.")
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
  
  conversion <- function(col, mult) {
    ## make sure only to convert those values that are not -999
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
    if (yrs[y] < strftime(start_date, "%Y")) {
      print(paste0(yrs[y], "<", strftime(start_date, "%Y")))
      next
    }
    if (yrs[y] > strftime(end_date, "%Y")) {
      print(paste0(yrs[y], ">", strftime(end_date, "%Y")))
      next
    }
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
        logger.warn("Could not find SLZ in Y file, making a crude assumpution.")
        slzdata <- array(c(-2.00, -1.50, -1.00, -0.80, -0.60, -0.40, -0.20, -0.10, -0.05))
      }
      
      ## store for later use, will only use last data
      dz <- diff(slzdata)
      dz <- dz[dz != 0.0]
      
      ## out <- add(getHdf5Data(ncT, 'TOTAL_AGB,1,row, yrs[y]) ## AbvGrndWood
      out <- add(getHdf5Data(ncT, 'FMEAN_BDEAD_PY'),1,row, yrs[y]) ## AbvGrndWood
      out <- add(getHdf5Data(ncT, 'FMEAN_PLRESP_PY'),2,row, yrs[y]) ## AutoResp
      out <- add(-999,3,row, yrs[y]) ## CarbPools
      out <- add(getHdf5Data(ncT, 'FMEAN_CAN_CO2_PY'),4,row, yrs[y]) ## CO2CAS
      out <- add(-999,5,row, yrs[y]) ## CropYield
      out <- add(getHdf5Data(ncT, 'FMEAN_GPP_PY'),6,row, yrs[y]) ## GPP
      out <- add(getHdf5Data(ncT, 'FMEAN_RH_PY'),7,row, yrs[y]) ## HeteroResp
      out <- add(getHdf5Data(ncT, 'FMEAN_GPP_PY') - getHdf5Data(ncT, 'FMEAN_PLRESP_PY') - getHdf5Data(ncT, 'FMEAN_RH_PY'),8,row, yrs[y]) ## NEE
      out <- add(getHdf5Data(ncT, 'FMEAN_GPP_PY') - getHdf5Data(ncT, 'FMEAN_PLRESP_PY'),9,row, yrs[y]) ## NPP
      out <- add(getHdf5Data(ncT, 'FMEAN_RH_PY') + getHdf5Data(ncT, 'FMEAN_PLRESP_PY'),10,row, yrs[y]) ## TotalResp
      ## out <- add(getHdf5Data(ncT, 'BDEAD + getHdf5Data(ncT, 'BALIVE,11,row, yrs[y]) ## TotLivBiom
      out <- add(-999,11,row, yrs[y]) ## TotLivBiom
      out <- add(getHdf5Data(ncT, 'FAST_SOIL_C_PY')+getHdf5Data(ncT, 'STRUCT_SOIL_C_PY')+getHdf5Data(ncT, 'SLOW_SOIL_C_PY'),12,row, yrs[y]) ## TotSoilCarb
      
      ## depth from surface to frozen layer
      tdepth <- 0
      fdepth <- 0
      soiltemp <- getHdf5Data(ncT, 'FMEAN_SOIL_TEMP_PY')
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
        fdepth <- array(0, ncol(soiltemp))
        tdepth <- array(0, ncol(soiltemp))
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
      
      out <- add(fdepth,13,row, yrs[y]) ## Fdepth
      out <- add(getHdf5Data(ncT, 'FMEAN_SFCW_DEPTH_PY'),14,row, yrs[y]) ## SnowDepth (ED2 currently groups snow in to surface water)
      out <- add(1-getHdf5Data(ncT, 'FMEAN_SFCW_FLIQ_PY'),15,row, yrs[y]) ## SnowFrac (ED2 currently groups snow in to surface water)
      out <- add(tdepth,16,row, yrs[y]) ## Tdepth
      out <- add(getHdf5Data(ncT, 'FMEAN_ATM_CO2_PY'),17,row, yrs[y]) ## CO2air
      out <- add(getHdf5Data(ncT, 'FMEAN_ATM_RLONG_PY'),18,row, yrs[y]) ## Lwdown
      out <- add(getHdf5Data(ncT, 'FMEAN_ATM_PRSS_PY'),19,row, yrs[y]) ## Psurf
      out <- add(getHdf5Data(ncT, 'FMEAN_ATM_SHV_PY'),20,row, yrs[y]) ## Qair
      out <- add(getHdf5Data(ncT, 'FMEAN_PCPG_PY'),21,row, yrs[y]) ## Rainf
      ##out <- add(getHdf5Data(ncT, 'AVG_NIR_BEAM') +
      ##           getHdf5Data(ncT, 'AVG_NIR_DIFFUSE')+
      ##           getHdf5Data(ncT, 'AVG_PAR_BEAM')+
      ##           getHdf5Data(ncT, 'AVG_PAR_DIFFUSE'),22,row, yrs[y]) ## Swdown
      ##out <- add(getHdf5Data(ncT, 'FMEAN_PAR_L_BEAM_PY')+
      ##           getHdf5Data(ncT, 'FMEAN_PAR_L_DIFF_PY'),22,row, yrs[y]) ## Swdown
      out <- add(getHdf5Data(ncT, 'FMEAN_ATM_PAR_PY'),22,row, yrs[y])  ## Swdown
      out <- add(getHdf5Data(ncT, 'FMEAN_ATM_TEMP_PY'),23,row, yrs[y]) ## Tair
      out <- add(getHdf5Data(ncT, 'FMEAN_ATM_VELS_PY'),24,row, yrs[y]) ## Wind
      ##out <- add(getHdf5Data(ncT, 'FMEAN_ATM_RLONG_PY')-getHdf5Data(ncT, 'AVG_RLONGUP'),25,row, yrs[y]) ## Lwnet
      out <- add(-999,25,row, yrs[y]) ## Lwnet
      ##out <- add(getHdf5Data(ncT, 'AVG_SENSIBLE_GC') + getHdf5Data(ncT, 'AVG_VAPOR_GC')*2272000,26,row, yrs[y]) ## Qg
      out <- add(-999,26,row, yrs[y]) ## Qg
      ##out <- add(getHdf5Data(ncT, 'AVG_SENSIBLE_TOT'),27,row, yrs[y]) ## Qh
      out <- add(getHdf5Data(ncT, 'FMEAN_SENSIBLE_AC_PY'),27,row, yrs[y]) ## Qh
      out <- add(getHdf5Data(ncT, 'FMEAN_VAPOR_LC_PY')+getHdf5Data(ncT, 'FMEAN_VAPOR_WC_PY')+getHdf5Data(ncT, 'FMEAN_VAPOR_GC_PY'),28,row, yrs[y]) ## Qle
      out <- add(-999,29,row, yrs[y]) ## Swnet
      out <- add(-999,30,row, yrs[y]) ## RootMoist
      out <- add(getHdf5Data(ncT, 'FMEAN_TRANSP_PY'),31,row, yrs[y]) ## Tveg
      out <- add(getHdf5Data(ncT, 'ZBAR'),32,row, yrs[y]) ## WaterTableD
      out <- add(-999,33,row, yrs[y]) ## fPAR
      ##lai <- matrix(apply(getHdf5Data(ncT, 'LAI_PFT'),1,sum,na.rm=TRUE),nrow=block)
      ## out <- add(lai,34,row, yrs[y]) ## LAI******************
      out <- add(getHdf5Data(ncT, 'FMEAN_LAI_PY'),34,row, yrs[y]) ## LAI
      ##z <- getHdf5Data(ncT, 'SLZ')
      ##if(z[length(z)] < 0.0) z <- c(z,0.0)
      ##dz <- diff(z)
      ##dz <- dz[dz != 0.0]
      ##fliq <- sum(getHdf5Data(ncT, 'AVG_SOIL_FRACLIQ')*dz)/-min(z)
      fliq <- NA#getHdf5Data(ncT, 'FMEAN_SFCW_FLIQ_PY')
      out <- add(1-fliq,35,row, yrs[y]) ## SMFrozFrac
      out <- add(fliq,36,row, yrs[y]) ## SMLiqFrac
      ## This needs to be soil wetness, i.e. multilple levels deep
      out <- add(getHdf5Data(ncT, 'FMEAN_SOIL_WATER_PY'),37,row, yrs[y]) ## SoilWater  **********
      ##out <- add(sum(soiltemp*dz)/-min(z),38) ## SoilTemp
      out <- add(soiltemp,38,row, yrs[y]) ## SoilTemp
      out <- add(-999,39,row, yrs[y]) ## SoilWet
      out <- add(getHdf5Data(ncT, 'FMEAN_ALBEDO_PY'),40,row, yrs[y]) ## Albedo
      out <- add(getHdf5Data(ncT, 'FMEAN_SFCW_TEMP_PY'),41,row, yrs[y]) ## SnowT (ED2 currently groups snow in to surface water)
      out <- add(getHdf5Data(ncT, 'FMEAN_SFCW_MASS_PY'),42,row, yrs[y]) ## SWE (ED2 currently groups snow in to surface water)
      out <- add(getHdf5Data(ncT, 'FMEAN_LEAF_TEMP_PY'),43,row, yrs[y]) ## VegT
      out <- add(getHdf5Data(ncT, 'FMEAN_VAPOR_LC_PY')+getHdf5Data(ncT, 'FMEAN_VAPOR_WC_PY')+getHdf5Data(ncT, 'FMEAN_VAPOR_GC_PY')+getHdf5Data(ncT, 'FMEAN_TRANSP_PY'),44,row, yrs[y]) ## Evap
      out <- add(getHdf5Data(ncT, 'FMEAN_QRUNOFF_PY'),45,row, yrs[y]) ## Qs
      out <- add(getHdf5Data(ncT, 'BASEFLOW'),46,row, yrs[y]) ## Qsb
      
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

    ## figure out what start day is, if not same year as start_date then it is 1
    if (yrs[y] == strftime(start_date, "%Y")) {
      start <- (as.numeric(strftime(start_date, "%j")) - 1) * block
    } else {
      start <- 0
    }
    if (yrs[y] == strftime(end_date, "%Y")) {
      end <- (as.numeric(strftime(end_date, "%j"))) * block - 1
    } else {
      end <- (as.numeric(strftime(paste0(yrs[y], "-12-31"), "%j"))) * block - 1
    }

    t <- ncdim_def(name = "time",
                   units = paste0("days since ", yrs[y], "-01-01 00:00:00"),
                   vals = start:end / block,
                   calendar = "standard", unlim = TRUE)
    lat <- ncdim_def("lat", "degrees_east",
                     vals =  as.numeric(sitelat),
                     longname = "station_latitude") 
    lon <- ncdim_def("lon", "degrees_north",
                     vals = as.numeric(sitelon),
                     longname = "station_longitude")

    zg <- ncdim_def("SoilLayerMidpoint", "meters", c(slzdata[1:length(dz)] + dz / 2, 0))
    
    ## Conversion factor for umol C -> kg C
    Mc <- 12.017 #molar mass of C, g/mol
    umol2kg_C <- Mc * ud.convert(1, "umol", "mol") * ud.convert(1, "g", "kg")

    var <- list()
    out <- conversion( 1, ud.convert(1, "t ha-1", "kg m-2"))     ## tC/ha     -> kg/m2
    var[[1]]  <- mstmipvar("AbvGrndWood", lat, lon, t, zg)
    out <- conversion( 2, umol2kg_C)  ## umol/m2 s-1 -> kg/m2 s-1
    var[[2]]  <- mstmipvar("AutoResp", lat, lon, t, zg)
    var[[3]]  <- mstmipvar("CarbPools", lat, lon, t, zg)
    var[[4]]  <- mstmipvar("CO2CAS", lat, lon, t, zg)
    var[[5]]  <- mstmipvar("CropYield", lat, lon, t, zg)
    out <- conversion( 6, umol2kg_C)  ## umol/m2 s-1 -> kg m-2 s-1
    var[[6]]  <- mstmipvar("GPP", lat, lon, t, zg)
    out <- conversion( 7, umol2kg_C)  ## umol/m2 s-1 -> kg m-2 s-1
    var[[7]]  <- mstmipvar("HeteroResp", lat, lon, t, zg)
    out <- conversion( 8, umol2kg_C)  ## umol/m2 s-1 -> kg m-2 s-1
    var[[8]]  <- mstmipvar("NEE", lat, lon, t, zg)
    out <- conversion( 9, umol2kg_C)  ## umol/m2 s-1 -> kg m-2 s-1
    var[[9]]  <- mstmipvar("NPP", lat, lon, t, zg)
    out <- conversion(10, umol2kg_C)  ## umol/m2 s-1 -> kg m-2 s-1
    var[[10]] <- mstmipvar("TotalResp", lat, lon, t, zg)
    var[[11]] <- mstmipvar("TotLivBiom", lat, lon, t, zg)
    var[[12]] <- mstmipvar("TotSoilCarb", lat, lon, t, zg)
    var[[13]] <- mstmipvar("Fdepth", lat, lon, t, zg)
    var[[14]] <- mstmipvar("SnowDepth", lat, lon, t, zg)
    var[[15]] <- mstmipvar("SnowFrac", lat, lon, t, zg)
    var[[16]] <- mstmipvar("Tdepth", lat, lon, t, zg)
    var[[17]] <- mstmipvar("CO2air", lat, lon, t, zg)
    var[[18]] <- mstmipvar("Lwdown", lat, lon, t, zg)
    var[[19]] <- mstmipvar("Psurf", lat, lon, t, zg)
    var[[20]] <- mstmipvar("Qair", lat, lon, t, zg)
    var[[21]] <- mstmipvar("Rainf", lat, lon, t, zg)
    var[[22]] <- mstmipvar("Swdown", lat, lon, t, zg)
    out <- checkTemp(23)
    var[[23]] <- mstmipvar("Tair", lat, lon, t, zg)
    var[[24]] <- mstmipvar("Wind", lat, lon, t, zg)
    var[[25]] <- mstmipvar("Lwnet", lat, lon, t, zg)
    var[[26]] <- mstmipvar("Qg", lat, lon, t, zg)
    var[[27]] <- mstmipvar("Qh", lat, lon, t, zg)
    var[[28]] <- mstmipvar("Qle", lat, lon, t, zg)
    var[[29]] <- mstmipvar("Swnet", lat, lon, t, zg)
    var[[30]] <- mstmipvar("RootMoist", lat, lon, t, zg)
    var[[31]] <- mstmipvar("Tveg", lat, lon, t, zg)
    var[[32]] <- mstmipvar("WaterTableD", lat, lon, t, zg)
    var[[33]] <- mstmipvar("fPAR", lat, lon, t, zg)
    var[[34]] <- mstmipvar("LAI", lat, lon, t, zg)
    ##var[[35]] <- mstmipvar("SMFrozFrac", lat, lon, t, zg)
    ##var[[36]] <- mstmipvar("SMLiqFrac", lat, lon, t, zg)
    var[[35]] <- mstmipvar("SMFrozFrac", lat, lon, t, zg)
    var[[36]] <- mstmipvar("SMLiqFrac", lat, lon, t, zg)
    var[[37]] <- mstmipvar("SoilMoist", lat, lon, t, zg)
    out <- checkTemp(38)
    var[[38]] <- mstmipvar("SoilTemp", lat, lon, t, zg)
    var[[39]] <- mstmipvar("SoilWet", lat, lon, t, zg)
    var[[40]] <- mstmipvar("Albedo", lat, lon, t, zg)
    out <- checkTemp(41)
    var[[41]] <- mstmipvar("SnowT", lat, lon, t, zg)
    var[[42]] <- mstmipvar("SWE", lat, lon, t, zg)
    out <- checkTemp(43)
    var[[43]] <- mstmipvar("VegT", lat, lon, t, zg)
    var[[44]] <- mstmipvar("Evap", lat, lon, t, zg)
    var[[45]] <- mstmipvar("Qs", lat, lon, t, zg)
    var[[46]] <- mstmipvar("Qsb", lat, lon, t, zg)


    ## write ALMA
    nc <- nc_create(file.path(outdir, paste(yrs[y], "nc", sep=".")), var)
    varfile <- file(file.path(outdir, paste(yrs[y], "nc", "var", sep=".")), "w")
    for(i in 1:length(var)) {
      ncvar_put(nc,var[[i]],out[[i]])
      cat(paste(var[[i]]$name, var[[i]]$longname), file=varfile, sep="\n")
    }
    close(varfile)
    nc_close(nc)
  }  ## end year loop
}  ## end model2netcdf.ED2
##==================================================================================================#

####################################################################################################
### EOF.  End of R script file.              
####################################################################################################
