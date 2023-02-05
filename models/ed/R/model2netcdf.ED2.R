#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------


##' Code to convert ED2's -T- HDF5 output into netCDF format
##' 
##' Modified from code to convert ED2's HDF5 output into the NACP
##' Intercomparison format (ALMA using netCDF)
##'
##' @param outdir Location of ED model output (e.g. a path to a single ensemble output)
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @param pfts a named vector of PFT numbers where the names are PFT names
##' @param settings pecan settings object
##' @param process_partial should failed runs be processed? Defaults to `FALSE`.
##'   `TRUE` will generate .nc files for runs that have generated some, but not
##'   all, of the expected outputs
##'
##' @details if \code{settings} is provided, then values for missing arguments
##'   `sitelat`, `sitelon`, `start_date`, `end_date`, and `pfts` will be taken
##'   from it
##'
##' @author Michael Dietze, Shawn Serbin, Rob Kooper, Toni Viskari, Istem Fer
## modified M. Dietze 07/08/12 modified S. Serbin 05/06/13
## refactored by Istem Fer on 03/2018
## further modified by S. Serbin 09/2018
##' @export
##'
model2netcdf.ED2 <- function(outdir,
                             sitelat,
                             sitelon,
                             start_date,
                             end_date,
                             pfts,
                             settings = NULL,
                             process_partial = FALSE) {
  if(!is.null(settings)) {
    if(!inherits(settings, "Settings")) {
      PEcAn.logger::logger.error("`settings` should be a PEcAn 'Settings' object")
    }
    if(missing(sitelat)) sitelat <- settings$run$site$lat
    if(missing(sitelon)) sitelon <- settings$run$site$lon
    if(missing(start_date)) start_date <- settings$run$start.date
    if(missing(end_date)) end_date <- settings$run$end.date
    if(missing(pfts)) pfts <- extract_pfts(settings$pfts)
  }
  
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)
  
  flist <- list()
  flist[["-T-"]] <- dir(outdir, "-T-") # tower files
  flist[["-E-"]] <- dir(outdir, "-E-") # monthly files

  # check if there are files
  file.check <- sapply(flist, function (f) length(f) != 0)

  if (!any(file.check)) {

    # no output files
    PEcAn.logger::logger.warn("WARNING: No output files found for :", outdir)
    return(NULL)

  } else {

    # which output files are there
    ed_res_flag <- names(flist)[file.check]

    # extract year info from the file names
    ylist <- lapply(
      ed_res_flag,
      function(x) stringr::str_extract(flist[[x]], "\\d{4}")
    )

    names(ylist) <- ed_res_flag
  }

  # prepare list to collect outputs
  out_list <- vector("list", length(ed_res_flag))
  names(out_list) <- ed_res_flag

  # If run failed there might be less years, no output case is handled above we
  # can process whatever is there, but of course this upsets ensemble.ts because
  # the outputs are not of same length now.
  # Two options:
  # (i)  don't process anything
  #      return(NULL)
  # (ii) check whether this is an ensemble run, then return null, otherwise
  # process whatever there is
  # for now I'm going with this, do failed runs also provide information
  # on parameters? 
  year_check <- unique(unlist(ylist))
  if (max(year_check) < end_year) { #if run failed early
    PEcAn.logger::logger.warn("Run ended earlier than expected.  Check logfile.txt")
    
    #figure out if this is an ensemble
    run_id <- basename(outdir)
    workflow_dir <- dirname(dirname(outdir))
    rundir <- file.path(workflow_dir, "run", run_id) 
    readme <- file.path(rundir, "README.txt")
    runtype <- readLines(readme, n = 1)
    is_ensemble <- grepl("ensemble", runtype)
    if (is_ensemble & !process_partial) {
      PEcAn.logger::logger.info("This is an ensemble run. ",
                                "Not processing anything.")
      return(NULL)
    } else {
      PEcAn.logger::logger.info("Processing existing outputs.")
      end_year <- max(year_check)
    }
  }
  
  # ----- start loop over years
  for (y in start_year:end_year) {

    PEcAn.logger::logger.info(paste0("----- Processing year: ", y))

    # ----- read values from ED output files
    for (i in seq_along(out_list)) {
      rflag <- ed_res_flag[i]
      # fcnx is either read_T_files() or read_E_files()
      fcnx  <- paste0("read_", gsub("-", "", rflag), "_files")
      out_list[[rflag]] <- do.call(fcnx, list(yr = y, ylist[[rflag]], flist[[rflag]],
                                              outdir, start_date, end_date,
                                              pfts, settings))
    }

    # generate start/end dates for processing
    if (y == start_year) {
      start_date_real <- lubridate::ymd(start_date)
    } else {
      #When would this be run?
      start_date_real <- lubridate::make_date(y, 1, 1)
    }

    if (y == end_year) {
      end_date_real <- lubridate::ymd(end_date)
    } else {
      #When would this be run?
      end_date_real <- lubridate::make_date(y, 12, 31)
    }

    # create lat/long nc variables
    lat <- ncdf4::ncdim_def("lat", "degrees_north",
                            vals = as.numeric(sitelat),
                            longname = "station_latitude")
    lon <- ncdf4::ncdim_def("lon", "degrees_east",
                            vals = as.numeric(sitelon),
                            longname = "station_longitude")

    # ----- put values to nc_var list
    nc_var <- list()
    for (i in seq_along(out_list)) {
      rflag   <- ed_res_flag[i]
      #fcnx is either put_T_values() or put_E_values()
      fcnx    <- paste0("put_", gsub("-", "", rflag), "_values")
      put_out <- do.call(fcnx, list(yr = y, nc_var = nc_var, var_list = out_list[[rflag]],
                                    lat = lat, lon = lon, start_date = start_date_real,
                                    end_date = end_date_real))
      nc_var            <- put_out$nc_var
      out_list[[rflag]] <- put_out$out
    }

    # ----- write ncdf files
    PEcAn.logger::logger.info("*** Writing netCDF file ***")

    out <- unlist(out_list, recursive = FALSE)
    #create nc file with slots for all variables
    nc <- ncdf4::nc_create(file.path(outdir, paste(y, "nc", sep = ".")),
                           nc_var)
    # define time_bounds for -T- outputs, if exists
    if (file.check[["-T-"]]==TRUE) {
      ncdf4::ncatt_put(nc, "time", "bounds", "time_bounds", prec = NA)
    }
    # define time_bounds for -E- outputs, if exists
    if (file.check[["-E-"]]==TRUE) {
      ncdf4::ncatt_put(nc, "dtime", "bounds", "dtime_bounds", prec = NA)
    }
    varfile <- file(file.path(outdir, paste(y, "nc", "var", sep = ".")), "w")
    # fill nc file with data
    for (i in seq_along(nc_var)) {
      var_put(nc, varid = nc_var[[i]], vals = out[[i]])
      cat(paste(nc_var[[i]]$name, nc_var[[i]]$longname), file = varfile,
          sep = "\n")
    }
    ncdf4::nc_close(nc)
    close(varfile)
  } # end year-loop

} # model2netcdf.ED2
##-------------------------------------------------------------------------------------------------#



##-------------------------------------------------------------------------------------------------#

##' Function for reading -T- files
##'
##' @details
##'  e.g.    yr = 1999
##'      yfiles = 1999 2000
##'      h5_files = "analysis-T-1999-00-00-000000-g01.h5" "analysis-T-2000-00-00-000000-g01.h5"
##'
##' @param yr the year being processed
##' @param yfiles the years on the filenames, will be used to matched h5_files for that year
##' @param h5_files names of T files to be read
##' @param outdir directory where ED2 output files are found
##' @param start_date start date in YYYY-MM-DD format
##' @param end_date end date in YYYY-MM-DD format
##' @param pfts for consistency with [read_E_files()]---unused
##' @param settings A PEcAn settings object. Values for `start_date` and
##'   `end_date` will be taken from `settings` if it is supplied.
##' 
##' @export
read_T_files <-
  function(yr,
           yfiles,
           h5_files,
           outdir,
           start_date,
           end_date,
           pfts = NULL,
           settings = NULL) {
    

  PEcAn.logger::logger.info(paste0("*** Reading -T- file ***"))
  if (!is.null(settings)) {
    if(!inherits(settings, "Settings")) {
      PEcAn.logger::logger.error("`settings` should be a PEcAn 'Settings' object")
    }
    if(missing(start_date)) start_date <- settings$run$start.date
    if(missing(end_date)) end_date <- settings$run$end.date
  }
  #TODO: rename add() to something more descriptive
  # add
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
          out[[col]] <- array(dat, dim = (end - start))
        } else {
          if (start != 0) {
            PEcAn.logger::logger.warn("start date is not 0 this year, 
                                      but data already exists in this col", 
                                      col, "how is this possible?")
          }
          out[[col]] <- abind::abind(out[[col]], 
                                     array(dat, dim = (end - start)), 
                                     along = 1)
        }
      } else {
        PEcAn.logger::logger.warn("expected a single value")
      }
    } else if (length(dims) == 1) {
      dat <- dat[1:(end - start)]
      if (length(out) < col) {
        out[[col]] <- dat
      } else {
        if (start != 0) {
          PEcAn.logger::logger.warn("start date is not 0 this year, 
                                    but data already exists in this col",
                                    col, "how is this possible?")
        }
        out[[col]] <- abind::abind(out[[col]], dat, along = 1)
      }
    } else if (length(dims) == 2) {
      dat <- t(dat)
      dims <- dim(dat)
      dat <- dat[1:(end - start), ]
      if (length(out) < col) {
        out[[col]] <- dat
      } else {
        if (start != 0) {
          PEcAn.logger::logger.warn("start date is not 0 this year, 
                                    but data already exists in this 
                                    col", col, "how is this possible?")
        }
        out[[col]] <- abind::abind(out[[col]], dat, along = 1)
      }
    } else {
      PEcAn.logger::logger.debug("-------------------------------------------------------------")
      PEcAn.logger::logger.debug("col=", col)
      PEcAn.logger::logger.debug("length=", length(dat))
      PEcAn.logger::logger.debug("start=", start)
      PEcAn.logger::logger.debug("end=", end)
      PEcAn.logger::logger.debug("dims=", dims)
      PEcAn.logger::logger.warn("Don't know how to handle larger arrays yet.")
    }
    
    ## finally make sure we use -999 for invalid values
    out[[col]][is.null(out[[col]])] <- -999
    out[[col]][is.na(out[[col]])] <- -999
    
    return(out)
  } # end add-function
  
  
  getHdf5Data <- function(nc, var) {
    if (var %in% names(nc$var)) {
      return(ncdf4::ncvar_get(nc, var))
    } else {
      PEcAn.logger::logger.warn("Could not find", var, "in ed hdf5 output.")
      return(-999)
    }
  }
  
  CheckED2Variables <- function(nc) {
    vars_detected <- NULL
    name_convention <- NULL
  
    if ("FMEAN_BDEAD_PY" %in% names(nc$var)) {
      vars_detected <- c(vars_detected,"FMEAN_BDEAD_PY")
      name_convention <- "Contains_FMEAN"
    }
    if ("FMEAN_SOIL_TEMP_PY" %in% names(nc$var)) {
      vars_detected <- c(vars_detected, "FMEAN_SOIL_TEMP_PY")
      name_convention <- "Contains_FMEAN"
    }
    if(!is.null(vars_detected)){
      PEcAn.logger::logger.warn(paste("Found variable(s): ", paste(vars_detected, collapse = " "), ", now processing FMEAN* named variables. Note that varible naming conventions may change with ED2 version."))
    }
    return(name_convention)
  }

  # note that there is always one Tower file per year
  ysel <- which(yr == yfiles)
  
  if (yr < strftime(start_date, "%Y")) {
    PEcAn.logger::logger.info(yr, "<", strftime(start_date, "%Y"))
  }
  
  if (yr > strftime(end_date, "%Y")) {
    PEcAn.logger::logger.info(yr, ">", strftime(end_date, "%Y"))
  }
  
  n <- length(ysel)
  out <- list()
  row <- 1
  
  # note that there is always one Tower file per year
  ncT <- ncdf4::nc_open(file.path(outdir, h5_files[ysel]))
  
  ## determine timestep from HDF5 file
  block <- ifelse(lubridate::leap_year(yr) == TRUE,
                  ncT$dim$phony_dim_0$len / 366, # a leaper 
                  ncT$dim$phony_dim_0$len / 365) # non leap

  PEcAn.logger::logger.info(paste0("Output interval: ", 86400 / block, " sec"))
  
  
  if (file.exists(file.path(outdir, sub("-T-", "-Y-", h5_files[ysel])))) {
    ncY <- ncdf4::nc_open(file.path(outdir, sub("-T-", "-Y-", h5_files[ysel])))
    slzdata <- getHdf5Data(ncY, "SLZ")
    ncdf4::nc_close(ncY)
  } else {
    PEcAn.logger::logger.warn("Could not find SLZ in Y file, 
                              making a crude assumpution.")
    slzdata <- array(c(-2, -1.5, -1, -0.8, -0.6, -0.4, -0.2, -0.1, -0.05))
  }
  
  ## Check for what naming convention of ED2 vars we are using. May change with ED2 version. 
  ED2vc <- CheckED2Variables(ncT)
  
  ## store for later use, will only use last data
  dz <- diff(slzdata)
  dz <- dz[dz != 0]
  
  if (!is.null(ED2vc)) {
    #NOTE: Take great care editing this.  The order of values in `out` must
    #match the order of nc_vars in put_T_values()!
    ## out <- add(getHdf5Data(ncT, 'TOTAL_AGB,1,row, yr) ## AbvGrndWood
    out <- add(getHdf5Data(ncT, "FMEAN_BDEAD_PY"), 1, row, yr)  ## AbvGrndWood
    out <- add(getHdf5Data(ncT, "FMEAN_PLRESP_PY"), 2, row, yr)  ## AutoResp
    out <- add(-999, 3, row, yr)  ## CarbPools
    out <- add(getHdf5Data(ncT, "FMEAN_CAN_CO2_PY"), 4, row, yr)  ## CO2CAS
    out <- add(-999, 5, row, yr)  ## CropYield
    out <- add(getHdf5Data(ncT, "FMEAN_GPP_PY"), 6, row, yr)  ## GPP
    out <- add(getHdf5Data(ncT, "FMEAN_RH_PY"), 7, row, yr)  ## HeteroResp
    out <- add(-getHdf5Data(ncT, "FMEAN_GPP_PY") + getHdf5Data(ncT, "FMEAN_PLRESP_PY") + 
                 getHdf5Data(ncT, "FMEAN_RH_PY"), 8, row, yr)  ## NEE
    out <- add(getHdf5Data(ncT, "FMEAN_GPP_PY") - getHdf5Data(ncT, "FMEAN_PLRESP_PY"), 
               9, row, yr)  ## NPP
    out <- add(getHdf5Data(ncT, "FMEAN_RH_PY") + getHdf5Data(ncT, "FMEAN_PLRESP_PY"), 
               10, row, yr)  ## TotalResp
    ## out <- add(getHdf5Data(ncT, 'BDEAD + getHdf5Data(ncT, 'BALIVE,11,row, yr) ## TotLivBiom
    out <- add(-999, 11, row, yr)  ## TotLivBiom
    out <- add(getHdf5Data(ncT, "FAST_SOIL_C_PY") + getHdf5Data(ncT, "STRUCT_SOIL_C_PY") + 
                 getHdf5Data(ncT, "SLOW_SOIL_C_PY"), 12, row, yr)  ## TotSoilCarb
    
    ## depth from surface to frozen layer
    tdepth <- 0
    fdepth <- 0
    soiltemp <- getHdf5Data(ncT, "FMEAN_SOIL_TEMP_PY")
    if (length(dim(soiltemp)) == 3) {
      fdepth <- array(0, dim = dim(soiltemp)[1:2])
      tdepth <- array(0, dim = dim(soiltemp)[1:2])
      for (t in 1:dim(soiltemp)[1]) { # time
        for (p in 1:dim(soiltemp)[2]) { # polygon
          for (i in dim(soiltemp)[3]:2) { # depth
            if (fdepth[t, p] == 0 & soiltemp[t, p, i] < 273.15 & 
                soiltemp[t, p, i - 1] > 273.13) {
              fdepth[t, p] <- i
            }
            if (tdepth[t, p] == 0 & soiltemp[t, p, i] > 273.15 & 
                soiltemp[t, p, i - 1] < 273.13) {
              tdepth[t, p] <- i
            }
          }
          SLZ <- c(slzdata[t, ], 0)
          z1 <- (SLZ[fdepth[t, p] + 1] + SLZ[fdepth[t, p]]) / 2
          z2 <- (SLZ[fdepth[t, p]] + SLZ[fdepth[t, p] - 1]) / 2
          if (fdepth[t, p] > 0) {
            fdepth[t, p] <- z1 + (z2 - z1) * (273.15 - soiltemp[t, p, fdepth[t, p]]) / 
              (soiltemp[t, p, fdepth[t, p] - 1] - soiltemp[t, p, fdepth[t, p]])
          }
          if (tdepth[t, p] > 0) {
            SLZ <- c(slzdata[t, ], 0)
            z1 <- (SLZ[tdepth[t, p] + 1] + SLZ[tdepth[t, p]]) / 2
            z2 <- (SLZ[tdepth[t, p]] + SLZ[tdepth[t, p] - 1]) / 2
            tdepth[t, p] <- z1 + (z2 - z1) * (273.15 - soiltemp[t, p, tdepth[t, p]]) / 
              (soiltemp[t, p, tdepth[t, p] - 1] - soiltemp[t, p, tdepth[t, p]])
          }
        }
      }
    } else {
      # no polygons, just time vs depth?
      fdepth <- array(0, ncol(soiltemp))
      tdepth <- array(0, ncol(soiltemp))
      for (t in 1:ncol(soiltemp)) { # time
        for (d in 2:nrow(soiltemp)) { # depth
          if (fdepth[t] == 0 & soiltemp[d, t] < 273.15 & soiltemp[d - 1, t] > 273.13) {
            fdepth[t] <- d
          }
          if (tdepth[t] == 0 & soiltemp[d, t] > 273.15 & soiltemp[d - 1, t] < 273.13) {
            tdepth[t] <- d
          }
        }
        if (fdepth[t] > 0) {
          SLZ <- c(slzdata, 0)
          z1 <- (SLZ[fdepth[t] + 1] + SLZ[fdepth[t]]) / 2
          z2 <- (SLZ[fdepth[t]] + SLZ[fdepth[t] - 1]) / 2
          fdepth[t] <- z1 + (z2 - z1) * (273.15 - soiltemp[fdepth[t], t]) / 
            (soiltemp[fdepth[t] - 1, t] - soiltemp[fdepth[t], t])
        }
        if (tdepth[t] > 0) {
          SLZ <- c(slzdata, 0)
          z1 <- (SLZ[tdepth[t] + 1] + SLZ[tdepth[t]]) / 2
          z2 <- (SLZ[tdepth[t]] + SLZ[tdepth[t] - 1]) / 2
          tdepth[t] <- z1 + (z2 - z1) * (273.15 - soiltemp[tdepth[t], t]) / 
            (soiltemp[tdepth[t] - 1, t] - soiltemp[tdepth[t], t])
        }
      }
    }
    
    out <- add(fdepth, 13, row, yr)  ## Fdepth
    out <- add(getHdf5Data(ncT, "FMEAN_SFCW_DEPTH_PY"), 14, row, yr)  ## SnowDepth (ED2 currently groups snow in to surface water)
    out <- add(1 - getHdf5Data(ncT, "FMEAN_SFCW_FLIQ_PY"), 15, row, yr)  ## SnowFrac (ED2 currently groups snow in to surface water)
    out <- add(tdepth, 16, row, yr)  ## Tdepth
    out <- add(getHdf5Data(ncT, "FMEAN_ATM_CO2_PY"), 17, row, yr)  ## CO2air
    out <- add(getHdf5Data(ncT, "FMEAN_ATM_RLONG_PY"), 18, row, yr)  ## Lwdown
    out <- add(getHdf5Data(ncT, "FMEAN_ATM_PRSS_PY"), 19, row, yr)  ## Psurf
    out <- add(getHdf5Data(ncT, "FMEAN_ATM_SHV_PY"), 20, row, yr)  ## Qair
    out <- add(getHdf5Data(ncT, "FMEAN_PCPG_PY"), 21, row, yr)  ## Rainf
    ##out <- add(getHdf5Data(ncT, 'AVG_NIR_BEAM') +
    ##           getHdf5Data(ncT, 'AVG_NIR_DIFFUSE')+
    ##           getHdf5Data(ncT, 'AVG_PAR_BEAM')+
    ##           getHdf5Data(ncT, 'AVG_PAR_DIFFUSE'),22,row, yr) ## Swdown
    ##out <- add(getHdf5Data(ncT, 'FMEAN_PAR_L_BEAM_PY')+
    ##           getHdf5Data(ncT, 'FMEAN_PAR_L_DIFF_PY'),22,row, yr) ## Swdown
    out <- add(getHdf5Data(ncT, "FMEAN_ATM_PAR_PY"), 22, row, yr)  ## Swdown
    out <- add(getHdf5Data(ncT, "FMEAN_ATM_TEMP_PY"), 23, row, yr)  ## Tair
    out <- add(getHdf5Data(ncT, "FMEAN_ATM_VELS_PY"), 24, row, yr)  ## Wind
    ## out <- add(getHdf5Data(ncT, 'FMEAN_ATM_RLONG_PY')-getHdf5Data(ncT, 'AVG_RLONGUP'),25,row,
    ## yr) ## Lwnet
    out <- add(-999, 25, row, yr)  ## Lwnet
    ## out <- add(getHdf5Data(ncT, 'AVG_SENSIBLE_GC') + getHdf5Data(ncT,
    ## 'AVG_VAPOR_GC')*2272000,26,row, yr) ## Qg
    out <- add(-999, 26, row, yr)  ## Qg
    ## out <- add(getHdf5Data(ncT, 'AVG_SENSIBLE_TOT'),27,row, yr) ## Qh
    out <- add(getHdf5Data(ncT, "FMEAN_SENSIBLE_AC_PY"), 27, row, yr)  ## Qh
    out <- add(getHdf5Data(ncT, "FMEAN_VAPOR_LC_PY") + getHdf5Data(ncT, "FMEAN_VAPOR_WC_PY") + 
                 getHdf5Data(ncT, "FMEAN_VAPOR_GC_PY") + getHdf5Data(ncT, "FMEAN_TRANSP_PY"), 28, row, yr)  ## Qle
    out <- add(-999, 29, row, yr)  ## Swnet
    out <- add(-999, 30, row, yr)  ## RootMoist
    out <- add(getHdf5Data(ncT, "FMEAN_TRANSP_PY"), 31, row, yr)  ## Tveg
    out <- add(getHdf5Data(ncT, "ZBAR"), 32, row, yr)  ## WaterTableD
    out <- add(-999, 33, row, yr)  ## fPAR
    ##lai <- matrix(apply(getHdf5Data(ncT, 'LAI_PFT'),1,sum,na.rm=TRUE),nrow=block)
    ## out <- add(lai,34,row, yr) ## LAI******************
    ## out <- add(getHdf5Data(ncT, 'FMEAN_LAI_PY'),34,row, yr) ## LAI - no longer using FMEAN LAI
    
    ## OLD - to be deprecated
    #laidata <- getHdf5Data(ncT,"LAI_PY")
    #if(length(dim(laidata)) == 3){
    #  out <- add(apply(laidata,3,sum),34,row,yr)
    #} else {
    #  out <- add(-999,34,row, yr)
    #}
    
    # code changes proposed by MCD, tested by SPS 20160607
    laidata <- getHdf5Data(ncT, "LAI_PY")
    if (length(dim(laidata)) == 3) {
      laidata <- apply(laidata, 3, sum)
      out <- add(array(laidata, dim = length(laidata)), 34, row, yr)
    } else {
      out <- add(-999, 34, row, yr)
    }
    
    ##fliq <- sum(getHdf5Data(ncT, 'AVG_SOIL_FRACLIQ')*dz)/-min(z)
    fliq <- NA  #getHdf5Data(ncT, 'FMEAN_SFCW_FLIQ_PY')
    out <- add(1 - fliq, 35, row, yr)  ## SMFrozFrac
    out <- add(fliq, 36, row, yr)  ## SMLiqFrac
    ## This needs to be soil wetness, i.e. multilple levels deep
    out <- add(getHdf5Data(ncT, "FMEAN_SOIL_WATER_PY"), 37, row, yr)  ## SoilWater  **********
    ## out <- add(sum(soiltemp*dz)/-min(z),38) ## SoilTemp
    out <- add(soiltemp, 38, row, yr)  ## SoilTemp
    out <- add(-999, 39, row, yr)  ## SoilWet
    out <- add(getHdf5Data(ncT, "FMEAN_ALBEDO_PY"), 40, row, yr)  ## Albedo
    out <- add(getHdf5Data(ncT, "FMEAN_SFCW_TEMP_PY"), 41, row, yr)  ## SnowT (ED2 currently groups snow in to surface water)
    out <- add(getHdf5Data(ncT, "FMEAN_SFCW_MASS_PY"), 42, row, yr)  ## SWE (ED2 currently groups snow in to surface water)
    out <- add(getHdf5Data(ncT, "FMEAN_LEAF_TEMP_PY"), 43, row, yr)  ## VegT
    out <- add(getHdf5Data(ncT, "FMEAN_VAPOR_LC_PY") + getHdf5Data(ncT, "FMEAN_VAPOR_WC_PY") + 
                 getHdf5Data(ncT, "FMEAN_VAPOR_GC_PY") + getHdf5Data(ncT, "FMEAN_TRANSP_PY"), 44, 
               row, yr)  ## Evap
    out <- add(getHdf5Data(ncT, "FMEAN_QRUNOFF_PY"), 45, row, yr)  ## Qs
    out <- add(getHdf5Data(ncT, "BASEFLOW"), 46, row, yr)  ## Qsb
    
    out <- add(getHdf5Data(ncT, "FMEAN_ROOT_RESP_PY") + getHdf5Data(ncT, "FMEAN_ROOT_GROWTH_RESP_PY") + 
                 getHdf5Data(ncT, "FMEAN_RH_PY"), 47, row, yr)  ## SoilResp
    out$SLZ <- slzdata
    
  } else {
    ## out <- add(getHdf5Data(ncT, 'TOTAL_AGB,1,row, yr) ## AbvGrndWood
    out <- add(getHdf5Data(ncT, "AVG_BDEAD"), 1, row, yr)  ## AbvGrndWood
    out <- add(getHdf5Data(ncT, "AVG_PLANT_RESP"), 2, row, yr)  ## AutoResp
    out <- add(-999, 3, row, yr)  ## CarbPools
    out <- add(getHdf5Data(ncT, "AVG_CO2CAN"), 4, row, yr)  ## CO2CAS
    out <- add(-999, 5, row, yr)  ## CropYield
    out <- add(getHdf5Data(ncT, "AVG_GPP"), 6, row, yr)  ## GPP
    out <- add(getHdf5Data(ncT, "AVG_HTROPH_RESP"), 7, row, yr)  ## HeteroResp
    out <- add(-getHdf5Data(ncT, "AVG_GPP") + getHdf5Data(ncT, "AVG_PLANT_RESP") + getHdf5Data(ncT, 
                                                                                               "AVG_HTROPH_RESP"), 8, row, yr)  ## NEE
    out <- add(getHdf5Data(ncT, "AVG_GPP") - getHdf5Data(ncT, "AVG_PLANT_RESP"), 9, row, 
               yr)  ## NPP
    out <- add(getHdf5Data(ncT, "AVG_HTROPH_RESP") + getHdf5Data(ncT, "AVG_PLANT_RESP"), 
               10, row, yr)  ## TotalResp
    ## out <- add(getHdf5Data(ncT, 'AVG_BDEAD + getHdf5Data(ncT, 'AVG_BALIVE,11,row, yr) ##
    ## TotLivBiom
    out <- add(-999, 11, row, yr)  ## TotLivBiom
    out <- add(getHdf5Data(ncT, "AVG_FSC") + getHdf5Data(ncT, "AVG_STSC") + 
                 getHdf5Data(ncT, "AVG_SSC"), 12, row, yr)  ## TotSoilCarb
    ## depth from surface to frozen layer
    tdepth <- 0
    fdepth <- 0
    soiltemp <- getHdf5Data(ncT, "AVG_SOIL_TEMP")
    if (length(dim(soiltemp)) == 3) {
      fdepth <- array(0, dim = dim(soiltemp)[1:2])
      tdepth <- array(0, dim = dim(soiltemp)[1:2])
      for (t in 1:dim(soiltemp)[1]) { # time
        for (p in 1:dim(soiltemp)[2]) { # polygon
          for (i in dim(soiltemp)[3]:2) { # depth
            if (fdepth[t, p] == 0 & soiltemp[t, p, i] < 273.15 & 
                soiltemp[t, p, i - 1] > 273.13) {
              fdepth[t, p] <- i
            }
            if (tdepth[t, p] == 0 & soiltemp[t, p, i] > 273.15 & 
                soiltemp[t, p, i - 1] < 273.13) {
              tdepth[t, p] <- i
            }
          }
          SLZ <- c(slzdata[t, ], 0)
          z1 <- (SLZ[fdepth[t, p] + 1] + SLZ[fdepth[t, p]]) / 2
          z2 <- (SLZ[fdepth[t, p]] + SLZ[fdepth[t, p] - 1]) / 2
          if (fdepth[t, p] > 0) {
            fdepth[t, p] <- z1 + (z2 - z1) * (273.15 - soiltemp[t, p, fdepth[t, p]]) / 
              (soiltemp[t, p, fdepth[t, p] - 1] - soiltemp[t, p, fdepth[t, p]])
          }
          if (tdepth[t, p] > 0) {
            SLZ <- c(slzdata[t, ], 0)
            z1 <- (SLZ[tdepth[t, p] + 1] + SLZ[tdepth[t, p]]) / 2
            z2 <- (SLZ[tdepth[t, p]] + SLZ[tdepth[t, p] - 1]) / 2
            tdepth[t, p] <- z1 + (z2 - z1) * (273.15 - soiltemp[t, p, tdepth[t, p]]) / 
              (soiltemp[t, p, tdepth[t, p] - 1] - soiltemp[t, p, tdepth[t, p]])
          }
        }
      }
    } else {
      # no polygons, just time vs depth?
      fdepth <- array(0, ncol(soiltemp))
      tdepth <- array(0, ncol(soiltemp))
      for (t in 1:ncol(soiltemp)) { # time
        for (d in 2:nrow(soiltemp)) { # depth
          if (fdepth[t] == 0 & soiltemp[d, t] < 273.15 & soiltemp[d - 1, t] > 273.13) {
            fdepth[t] <- d
          }
          if (tdepth[t] == 0 & soiltemp[d, t] > 273.15 & soiltemp[d - 1, t] < 273.13) {
            tdepth[t] <- d
          }
        }
        if (fdepth[t] > 0) {
          SLZ <- c(slzdata, 0)
          z1 <- (SLZ[fdepth[t] + 1] + SLZ[fdepth[t]]) / 2
          z2 <- (SLZ[fdepth[t]] + SLZ[fdepth[t] - 1]) / 2
          fdepth[t] <- z1 + (z2 - z1) * (273.15 - soiltemp[fdepth[t], t]) / 
            (soiltemp[fdepth[t] - 1, t] - soiltemp[fdepth[t], t])
        }
        if (tdepth[t] > 0) {
          SLZ <- c(slzdata, 0)
          z1 <- (SLZ[tdepth[t] + 1] + SLZ[tdepth[t]]) / 2
          z2 <- (SLZ[tdepth[t]] + SLZ[tdepth[t] - 1]) / 2
          tdepth[t] <- z1 + (z2 - z1) * (273.15 - soiltemp[tdepth[t], t]) / 
            (soiltemp[tdepth[t] - 1, t] - soiltemp[tdepth[t], t])
        }
      }
    }
    
    out <- add(fdepth, 13, row, yr)  ## Fdepth
    out <- add(getHdf5Data(ncT, "AVG_SNOWDEPTH"), 14, row, yr)  ## SnowDepth
    out <- add(1 - getHdf5Data(ncT, "AVG_SNOWFRACLIQ"), 15, row, yr)  ## SnowFrac
    out <- add(tdepth, 16, row, yr)  ## Tdepth
    out <- add(getHdf5Data(ncT, "AVG_ATM_CO2"), 17, row, yr)  ## CO2air
    out <- add(getHdf5Data(ncT, "AVG_RLONG"), 18, row, yr)  ## Lwdown
    out <- add(getHdf5Data(ncT, "AVG_PRSS"), 19, row, yr)  ## Psurf
    out <- add(getHdf5Data(ncT, "AVG_ATM_SHV"), 20, row, yr)  ## Qair
    out <- add(getHdf5Data(ncT, "AVG_PCPG"), 21, row, yr)  ## Rainf
    ##out <- add(getHdf5Data(ncT, 'AVG_NIR_BEAM') +
    ##           getHdf5Data(ncT, 'AVG_NIR_DIFFUSE')+
    ##           getHdf5Data(ncT, 'AVG_PAR_BEAM')+
    ##           getHdf5Data(ncT, 'AVG_PAR_DIFFUSE'),22,row, yr) ## Swdown
    out <- add(getHdf5Data(ncT, "AVG_PAR_BEAM") + getHdf5Data(ncT, "AVG_PAR_DIFFUSE"), 
               22, row, yr)  ## Swdown
    out <- add(getHdf5Data(ncT, "AVG_ATM_TMP"), 23, row, yr)  ## Tair
    out <- add(getHdf5Data(ncT, "AVG_VELS"), 24, row, yr)  ## Wind
    ##out <- add(getHdf5Data(ncT, 'AVG_RLONG')-getHdf5Data(ncT, 'AVG_RLONGUP'),25,row, yr) ## Lwnet
    out <- add(-999, 25, row, yr)  ## Lwnet
    ## out <- add(getHdf5Data(ncT, 'AVG_SENSIBLE_GC') + getHdf5Data(ncT,
    ## 'AVG_VAPOR_GC')*2272000,26,row, yr) ## Qg
    out <- add(-999, 26, row, yr)  ## Qg
    ## out <- add(getHdf5Data(ncT, 'AVG_SENSIBLE_TOT'),27,row, yr) ## Qh
    out <- add(getHdf5Data(ncT, "AVG_SENSIBLE_AC"), 27, row, yr)  ## Qh
    out <- add(getHdf5Data(ncT, "AVG_EVAP"), 28, row, yr)  ## Qle
    out <- add(-999, 29, row, yr)  ## Swnet
    out <- add(-999, 30, row, yr)  ## RootMoist
    out <- add(getHdf5Data(ncT, "AVG_TRANSP"), 31, row, yr)  ## Tveg
    out <- add(getHdf5Data(ncT, "ZBAR"), 32, row, yr)  ## WaterTableD
    out <- add(-999, 33, row, yr)  ## fPAR
    ##lai <- matrix(apply(getHdf5Data(ncT, 'LAI_PFT'),1,sum,na.rm=TRUE),nrow=block)
    ## out <- add(lai,34,row, yr) ## LAI******************
    out <- add(getHdf5Data(ncT, "LAI"), 34, row, yr)  ## LAI
    ##fliq <- sum(getHdf5Data(ncT, 'AVG_SOIL_FRACLIQ')*dz)/-min(z)
    fliq <- NA  #getHdf5Data(ncT, 'AVG_SOIL_FRACLIQ')
    out <- add(1 - fliq, 35, row, yr)  ## SMFrozFrac
    out <- add(fliq, 36, row, yr)  ## SMLiqFrac
    ## This needs to be soil wetness, i.e. multilple levels deep
    out <- add(getHdf5Data(ncT, "AVG_SOIL_WATER"), 37, row, yr)  ## SoilWater  **********
    ## out <- add(sum(soiltemp*dz)/-min(z),38) ## SoilTemp
    out <- add(soiltemp, 38, row, yr)  ## SoilTemp
    out <- add(-999, 39, row, yr)  ## SoilWet
    out <- add(getHdf5Data(ncT, "AVG_ALBEDO"), 40, row, yr)  ## Albedo
    out <- add(getHdf5Data(ncT, "AVG_SNOWTEMP"), 41, row, yr)  ## SnowT
    out <- add(getHdf5Data(ncT, "AVG_SNOWMASS"), 42, row, yr)  ## SWE
    out <- add(getHdf5Data(ncT, "AVG_VEG_TEMP"), 43, row, yr)  ## VegT
    out <- add(getHdf5Data(ncT, "AVG_EVAP") + getHdf5Data(ncT, "AVG_TRANSP"), 44, row, 
               yr)  ## Evap
    out <- add(getHdf5Data(ncT, "AVG_RUNOFF"), 45, row, yr)  ## Qs
    out <- add(getHdf5Data(ncT, "BASEFLOW"), 46, row, yr)  ## Qsb     
    out <- add(getHdf5Data(ncT, "AVG_ROOT_RESP") + getHdf5Data(ncT, "AVG_ROOT_MAINTENANCE") + 
                 getHdf5Data(ncT, "AVG_HTROPH_RESP"), 47, row, yr)  ## SoilResp
    out$SLZ <- slzdata
  }
  
  ncdf4::nc_close(ncT)

  return(out)
  
} # read_T_files


##-------------------------------------------------------------------------------------------------#

##' Function for put -T- values to nc_var list
##' 
##' @param yr the year being processed
##' @param nc_var a list (potentially empty) for `ncvar4` objects to be added to
##' @param var_list list returned by [read_E_files()]
##' @param lat `ncdim4` object for latitude of site
##' @param lon `ncdim4` object longitude of site
##' @param start_date start time of simulation
##' @param end_date end time of simulation
##' @param begins deprecated; use `start_date` instead
##' @param ends deprecated; use `end_date` instead
##' @param out deprecated; use `var_list` instead
##'
put_T_values <-
  function(yr,
           nc_var,
           var_list,
           lat,
           lon,
           start_date,
           end_date,
           begins,
           ends,
           out) {
    
  if(!missing(begins)) {
    warning("`begins` is deprecated, using `start_date` instead")
    start_date <- begins
  }
  if(!missing(ends)) {
    warning("`ends` is deprecated, using `end_date` instead")
    end_date <- ends
  }
  if(!missing(out)) {
    warning("`out` is deprecated, using `var_list` instead")
    var_list <- out
  }
  s <- length(nc_var)
  
  # create out list to be modified
  out <- var_list
  
  ## Conversion factor for umol C -> kg C
  Mc <- 12.017  #molar mass of C, g/mol
  umol2kg_C <- Mc * PEcAn.utils::ud_convert(1, "umol", "mol") * PEcAn.utils::ud_convert(1, "g", "kg")
  yr2s      <- PEcAn.utils::ud_convert(1, "s", "yr")
  
  # TODO - remove this function and replace with ifelse statements inline below (SPS)
  conversion <- function(col, mult) {
    ## make sure only to convert those values that are not -999
    out[[col]][out[[col]] != -999] <- out[[col]][out[[col]] != -999] * mult
    return(out)
  }
  
  checkTemp <- function(col) {
    out[[col]][out[[col]] == 0] <- -999
    return(out)
  }
  
  
  # ----- define ncdf dimensions
  #### setup output time and time bounds
  ## Create a date vector that contains each day of the model run for each output year (e.g. "2001-07-15", "2001-07-16"....)
  ## and which is the correct length for each full or partial year
  output_date_vector <- output_date_vector <-
    seq(
      lubridate::ymd(start_date),
      lubridate::ymd(end_date),
      by = "day",
    )
  ## Calculate model output frequency per day (e.g. 0.02083333)
  model_timestep_s <- length(output_date_vector) / length(out[[1]])
  iter_per_day <- round(1 / model_timestep_s) ## e.g. 48
  ## Create a timesteps vector (e.g. 0.00000000 0.02083333 0.04166667 0.06250000 0.08333333 0.10416667 ...)
  timesteps <- head(seq(0, 1, by = 1 / iter_per_day), -1)
  ## Create a new date vector where each day is repeated by iter_per_day 
  ## (e.g. "2001-07-15" "2001-07-15" "2001-07-15" "2001-07-15" "2001-07-15" ...)
  sub_dates <- rep(output_date_vector, each = iter_per_day)
  ## Generate a vector of julian dates from sub_dates (e.g. 196 196 196 196 196 196 ...)
  jdates <- lubridate::yday(sub_dates)
  ## Create a fractional DOY vector using jdates, subtract by 1 to be 0 index
  ## (e.g. 195.0000 195.0208 195.0417 195.0625 195.0833 195.1042)
  ## which yields, e.g. as.Date(195.0000,origin="2001-01-01"), "2001-07-15" 
  tvals <- (jdates + timesteps) - 1
  ## Create time bounds to populate time_bounds variable
  bounds <- array(data = NA, dim = c(length(tvals), 2))
  bounds[, 1] <- tvals
  bounds[, 2] <- bounds[, 1] + (1 / iter_per_day)
  bounds <- round(bounds, 4)  # create time bounds for each timestep in t, t+1; t+1, t+2... format
  ####
  
  t <- ncdf4::ncdim_def(name = "time", units = paste0("days since ", yr, "-01-01 00:00:00"), 
                        vals = tvals, 
                        calendar = "standard", unlim = TRUE)
  time_interval <- ncdf4::ncdim_def(name = "hist_interval", 
                                    longname = "history time interval endpoint dimensions", 
                                    vals = 1:2, units = "")
  
  slzdata <- out$SLZ
  dz <- diff(slzdata)
  dz <- dz[dz != 0]
  
  zg <- ncdf4::ncdim_def("SoilLayerMidpoint", "meters", c(slzdata[1:length(dz)] + dz / 2, 0))
  
  # currently unused
  #dims  <- list(lon = lon, lat = lat, time = t)
  #dimsz <- list(lon = lon, lat = lat, time = t, nsoil = zg)

  # ----- fill list
  
  out <- conversion(1, PEcAn.utils::ud_convert(1, "t ha-1", "kg m-2"))  ## tC/ha -> kg/m2
  nc_var[[s + 1]] <- ncdf4::ncvar_def("AbvGrndWood", units = "kg C m-2", dim = list(lon, lat, t), missval = -999, 
                                    longname = "Above ground woody biomass")
  out <- conversion(2, umol2kg_C)  ## umol/m2 s-1 -> kg/m2 s-1
  nc_var[[s + 2]] <- ncdf4::ncvar_def("AutoResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                    longname = "Autotrophic Respiration")
  nc_var[[s + 3]] <- ncdf4::ncvar_def("CarbPools", units = "kg C m-2", dim = list(lon, lat, t), missval = -999, 
                                    longname = "Size of each carbon pool")
  nc_var[[s + 4]] <- ncdf4::ncvar_def("CO2CAS", units = "ppmv", dim = list(lon, lat, t), missval = -999, 
                                    longname = "CO2CAS")
  nc_var[[s + 5]] <- ncdf4::ncvar_def("CropYield", units = "kg m-2", dim = list(lon, lat, t), missval = -999, 
                                    longname = "Crop Yield")
  out <- conversion(6, yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[[s + 6]] <- ncdf4::ncvar_def("GPP", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                    longname = "Gross Primary Productivity")
  out <- conversion(7, yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[[s + 7]] <- ncdf4::ncvar_def("HeteroResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                    longname = "Heterotrophic Respiration")
  out <- conversion(8, yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[[s + 8]] <-  ncdf4::ncvar_def("NEE", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Net Ecosystem Exchange")
  out <- conversion(9, yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[[s + 9]] <- ncdf4::ncvar_def("NPP", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                    longname = "Net Primary Productivity")
  out <- conversion(10, yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[[s + 10]] <- ncdf4::ncvar_def("TotalResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Total Respiration")
  nc_var[[s + 11]] <- ncdf4::ncvar_def("TotLivBiom", units = "kg C m-2", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Total living biomass")
  nc_var[[s + 12]] <- ncdf4::ncvar_def("TotSoilCarb", units = "kg C m-2", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Total Soil Carbon")
  nc_var[[s + 13]] <- ncdf4::ncvar_def("Fdepth", units = "m", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Frozen Thickness")
  nc_var[[s + 14]] <- ncdf4::ncvar_def("SnowDepth", units = "m", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Total snow depth")
  nc_var[[s + 15]] <- PEcAn.utils::mstmipvar("SnowFrac", lat, lon, t, zg) # not standard
  nc_var[[s + 16]] <- ncdf4::ncvar_def("Tdepth", units = "m", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Active Layer Thickness")
  nc_var[[s + 17]] <- ncdf4::ncvar_def("CO2air", units = "umol mol-1", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Near surface CO2 concentration")
  nc_var[[s + 18]] <- ncdf4::ncvar_def("LWdown", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Surface incident longwave radiation")
  nc_var[[s + 19]] <- ncdf4::ncvar_def("Psurf", units = "Pa", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Surface pressure")
  nc_var[[s + 20]] <- ncdf4::ncvar_def("Qair", units = "kg kg-1", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Near surface specific humidity")
  nc_var[[s + 21]] <- ncdf4::ncvar_def("Rainf", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Rainfall rate")
  nc_var[[s + 22]] <- ncdf4::ncvar_def("SWdown", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Surface incident shortwave radiation")
  out <- checkTemp(23)
  nc_var[[s + 23]] <- ncdf4::ncvar_def("Tair", units = "K", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Near surface air temperature")
  nc_var[[s + 24]] <- ncdf4::ncvar_def("Wind", units = "m s-1", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Near surface module of the wind")
  nc_var[[s + 25]] <- ncdf4::ncvar_def("LWnet", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Net Longwave Radiation")
  nc_var[[s + 26]] <- ncdf4::ncvar_def("Qg", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Ground heat")
  nc_var[[s + 27]] <- ncdf4::ncvar_def("Qh", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Sensible heat")
  out <- conversion(28, PEcAn.data.atmosphere::get.lv())  ## kg m-2 s-1 -> W m-2
  nc_var[[s + 28]] <- ncdf4::ncvar_def("Qle", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Latent heat")
  nc_var[[s + 29]] <- ncdf4::ncvar_def("SWnet", units = "W m-2", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Net shortwave radiation")
  nc_var[[s + 30]] <- PEcAn.utils::mstmipvar("RootMoist", lat, lon, t, zg)   # not standard
  nc_var[[s + 31]] <- ncdf4::ncvar_def("TVeg", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Transpiration")
  nc_var[[s + 32]] <- PEcAn.utils::mstmipvar("WaterTableD", lat, lon, t, zg) # not standard

  nc_var[[s + 33]] <- ncdf4::ncvar_def("fPAR", units = "", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Absorbed fraction incoming PAR")
  nc_var[[s + 34]] <- ncdf4::ncvar_def("LAI", units = "m2 m-2", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Leaf Area Index")
  nc_var[[s + 35]] <- PEcAn.utils::mstmipvar("SMFrozFrac", lat, lon, t, zg)  # not standard
  nc_var[[s + 36]] <- PEcAn.utils::mstmipvar("SMLiqFrac", lat, lon, t, zg)   # not standard
  nc_var[[s + 37]] <- ncdf4::ncvar_def("SoilMoist", units = "kg m-2", dim = list(lon, lat, zg, t), missval = -999, 
                                     longname = "Average Layer Soil Moisture")
  out <- checkTemp(38)
  nc_var[[s + 38]] <- ncdf4::ncvar_def("SoilTemp", units = "K", dim = list(lon, lat, zg, t), missval = -999, 
                                     longname = "Average Layer Soil Temperature")
  nc_var[[s + 39]] <- ncdf4::ncvar_def("SoilWet", units = "", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Total Soil Wetness")
  nc_var[[s + 40]] <- PEcAn.utils::mstmipvar("Albedo", lat, lon, t, zg)      # not standard
  out <- checkTemp(41)
  nc_var[[s + 41]] <- PEcAn.utils::mstmipvar("SnowT", lat, lon, t, zg)       # not standard
  nc_var[[s + 42]] <- ncdf4::ncvar_def("SWE", units = "kg m-2", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Snow Water Equivalent")
  out <- checkTemp(43)
  nc_var[[s + 43]] <- PEcAn.utils::mstmipvar("VegT", lat, lon, t, zg)        # not standard
  nc_var[[s + 44]] <- ncdf4::ncvar_def("Evap", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Total Evaporation")
  nc_var[[s + 45]] <- ncdf4::ncvar_def("Qs", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Surface runoff")
  nc_var[[s + 46]] <- ncdf4::ncvar_def("Qsb", units = "kg m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                     longname = "Subsurface runoff")
  out <- conversion(47, yr2s)  ## kg C m-2 yr-1 -> kg C m-2 s-1
  nc_var[[s + 47]] <- ncdf4::ncvar_def("SoilResp", units = "kg C m-2 s-1", dim = list(lon, lat, t), missval = -999, 
                                    longname = "Soil Respiration")
  # Remove SLZ from output before finalizing list.  replace with time_bounds
  if(!is.null(out[["SLZ"]])){
    out[["SLZ"]] <- NULL
  }
  out_length <- length(out)
  out[[out_length + 1]] <- c(rbind(bounds[, 1], bounds[, 2]))
  nc_var[[s + (out_length + 1)]] <- ncdf4::ncvar_def(name="time_bounds", units='', 
                                    longname = "history time interval endpoints", 
                                    dim=list(time_interval,time = t), 
                                    prec = "double")
  
  return(list(nc_var = nc_var, out = out))

} # put_T_values


##-------------------------------------------------------------------------------------------------#

##' Function for reading -E- files
##'
##' This function reads in monthly output (-E- .h5 files) from ED2, does unit
##' conversions, and returns a list to be passed to [put_E_values()].  Cohort
##' level variables (i.e. those ending in "_CO") are often (always?) in
##' per-plant units rather than per area. This function converts them to per
##' area using the plant density and patch area before converting units to PEcAn
##' standards.
##'
##' @param yr length 1 numeric vector; the year being processed
##' @param yfiles numeric vector of the years on the filenames, will be used to
##'   matched `h5_files` for that year
##' @param h5_files character vector of names of E h5 files (e.g.
##'   "analysis-E-1999-06-00-000000-g01.h5")
##' @param outdir directory where ED2 output files are found
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @param pfts a named vector of PFT numbers where the names are PFT names
##' @param settings pecan settings object
##' @export
##'
##' @details if \code{settings} is provided, then values for missing arguments
##'   for `start_date`, `end_date`, and `pfts` will be taken from it
##'   
##' @return a list
##' 
read_E_files <- function(yr, yfiles, h5_files, outdir, start_date, end_date, 
                         pfts, settings = NULL) {
  
  PEcAn.logger::logger.info(paste0("*** Reading -E- file ***"))
  
  if (!is.null(settings)) {
    if(!inherits(settings, "Settings")) {
      PEcAn.logger::logger.error("`settings` should be a PEcAn 'Settings' object")
    }
    if(missing(start_date)) start_date <- settings$run$start.date
    if(missing(end_date)) end_date <- settings$run$end.date
    if(missing(pfts)) pfts <- extract_pfts(settings$pfts)
  }
  
  stopifnot(!is.null(outdir), !is.null(start_date), !is.null(end_date), 
            !is.null(pfts))
  
  # there are multiple -E- files per year
  ysel <- which(yr == yfiles)
  
  # grab year-month info from file names, e.g. "199906"
  times <- gsub(
    "(.*)\\-(.*)\\-(.*)\\-(.*)\\-(.*)", "\\1\\2",
    sapply(
      strsplit(h5_files, "-E-"), 
      function(x) x[2] # Select only the part of each name after res.flag
    )
  )
  
  #Check that all the expected files exist using start_date and end_date
  expected_ym <- seq(
    lubridate::ymd(start_date),
    # an E file is only written if a month is completed.
    # E.g. start_date=2004-07-01, end_date=2004-08-31 will result in one E file for 2004-07
    lubridate::floor_date(lubridate::ymd(end_date), "month") - lubridate::days(1), 
    by = "month"
  ) %>% format("%Y%m")
  
  if(!all(expected_ym %in% times)) {
    #TODO: possibly not an error, but then need to use actual months of output files for time dimension in put_E_values(),  not start_date:end_date.
    PEcAn.logger::logger.error("Not all expected E files found!")
  }
  
  # lets make it work for a subset of vars fist
  # TODO :  read all (or more) variables, functionality exists, see below
  varnames <-
    c(
      "DBH", #diameter at breast height (cm)
      "DDBH_DT", #change in DBH (cm/plant/yr) 
      "AGB_CO", #cohort level above ground biomass (kgC/plant)
      "MMEAN_NPPDAILY_CO", #net primary productivity (kgC/m2/yr)
      "MMEAN_TRANSP_CO", #Monthly mean leaf transpiration (kg/m2/s)
      "BSEEDS_CO", #seed biomass in units of (kgC/plant)
      "NPLANT" #plant density (plants/m2), required for /plant -> /m2 conversion
    )
  
  # List of vars to extract includes the requested one, plus others needed below 
  vars <- c(
    varnames,
    "PFT"#, #pft numbers

    # "AREA", #patch area relative to site area (unitless)
    # "AREA_SI", #site area relative to polygon area (unitless)
    # "PACO_N" #number of cohorts in each patch
    )
  
  # list to collect outputs
  ed.dat <- list()
  
  # loop over the files for that year
  for(i in ysel) {
    
    nc <- ncdf4::nc_open(file.path(outdir, h5_files[i]))
    on.exit(ncdf4::nc_close(nc), add = FALSE)
    allvars <- names(nc$var)
    if (!is.null(vars)) allvars <- allvars[ allvars %in% vars ]
    
    # extract all the data into list
    #TODO warn if a variable isn't available and return -999 (?)
    if (length(ed.dat) == 0){
      for (j in 1:length(allvars)) {
        ed.dat[[j]] <- list()
        ed.dat[[j]][[1]] <- ncdf4::ncvar_get(nc, allvars[j])
      }
      names(ed.dat) <- allvars
    } else {
      
      # 2nd and more months
      t <- length(ed.dat[[1]]) + 1
      
      for (j in 1:length(allvars)) {
        
        k <- which(names(ed.dat) == allvars[j])
        
        if (length(k)>0) {
          
          ed.dat[[k]][[t]] <- ncdf4::ncvar_get(nc, allvars[j])
          
        } else { ## add a new ed.datiable. ***Not checked (shouldn't come up?)
          
          ed.dat[[length(ed.dat)+1]] <- list()    # Add space for new ed.datiable
          ed.dat[[length(ed.dat)]][1:(t-1)] <- NA # Give NA for all previous time points
          ed.dat[[length(ed.dat)]][t] <- ncdf4::ncvar_get(nc, allvars[j]) # Assign the value of the new ed.datiable at this time point
          names(ed.dat)[[length(ed.dat)]] <- allvars[j] 
          
        }
      }      
    }
    
  } # end ysel-loop

  
  # even if this is a SA run for soil, currently we are not reading any variable
  # that has a soil dimension. "soil" will be passed to read.output as pft.name
  # from upstream, when it's not part of the attribute it will read the sum
  soil.check <- grepl("soil", names(pfts))
  if(any(soil.check)){
    # for now keep soil out
    #TODO: print a message??
    pfts <- pfts[!(soil.check)]
  }
  
  # Aggregate over PFT and DBH bins  
  for (i in seq_along(ysel)) {
 
    #At this point, every element in ed.dat is a list of variables each having
    #one element per month that is usually a vector but sometimes a matrix. For
    #each variable, the following needs to be mapped to to each month:
    #' 1) Is the variable a matrix?  If so, get colsums to turn it into a vector with one element per cohort
    #' 2) Is the variable in per-plant units? If so, it needs converting to per area units
    #' 3) Are the units PEcAn standard? If not, they need converting (e.g. with PEcAn.utils::ud_convert()) (make use of PEcAn.utils::standard.vars?)
    #' 4) group by PFT and sum cohorts
    
    #TODO: This is written in a way that only apply to cohort-level variables.  Should this be generalized?
    out <- 
      #TODO, this outer imap could be made into a for-loop if it makes it easier for people to read and edit in the future.  not necessarily faster with imap
      purrr::imap(ed.dat[names(ed.dat) %in% varnames], ~{ 
        #.x is elements of ed.dat and .y is names of ed.dat
        #1) collapse matrix variables into vector
        if (all(purrr::map_lgl(.x, is.matrix))) {
          var <- purrr::map(.x, colSums)
        } else {
          var <- .x
        }
        
        #2) do per plant -> per area correction
        if (.y %in% c("BSEEDS_CO", "AGB_CO")) {
          var <- purrr::map2(var, ed.dat$NPLANT, `*`)
        }
        
        #3) convert units to PEcAn standard if necessary
        #input units are according to the ED2 source code: https://raw.githubusercontent.com/EDmodel/ED2/master/ED/src/memory/ed_state_vars.F90, output units are according to PEcAn.utils::standard_vars
        if(.y == "MMEAN_NPPDAILY_CO") {
          var <- purrr::map(var, ~ PEcAn.utils::ud_convert(.x, u1 = "kg/m2/yr", u2 = "kg/m2/s"))
        }

        var
      }) 
    
    #sum cohorts by PFT
    out <- 
      purrr::map(.x = out, #for each variable in `out`
          ~purrr::map2(.x = .x, .y = ed.dat$PFT, #for each month in each variable
                ~ tapply(.x, .y, sum) #sum variable by PFT number
          ))
  }
    
  #Bind rows for months together to produce a matrix with ncol = length(pfts) and nrow = number of months
  out <- purrr::map(out, ~do.call(rbind, .x))
  
  out$PFT <- pfts #named vector for matching PFT numbers to names
  
  #New varnames to match PEcAn standard
  names(out) <- dplyr::case_when(
                  #ED2 name             #PEcAN name
    names(out) == "AGB_CO"            ~ "AGB_PFT",
    names(out) == "BSEEDS_CO"         ~ "BSEEDS",
    names(out) == "DDBH_DT"           ~ "DDBH",
    names(out) == "MMEAN_NPPDAILY_CO" ~ "NPP_PFT",
    names(out) == "MMEAN_TRANSP_CO"   ~ "TRANSP_PFT",
    names(out) == "NPLANT"            ~ "DENS",
    TRUE ~ names(out)
  )
  
  return(out)
  
} # end read_E_files



##' Put -E- values to nc_var list
##' 
##' Puts a select number of variables from the monthly -E- files into a `nc_var`
##' list to be written to a .nc file.
##' 
##' @param yr the year being processed
##' @param nc_var a list (potentially empty) for `ncvar4` objects to be added to
##' @param var_list list returned by [read_E_files()]
##' @param lat `ncdim4` object for latitude of site
##' @param lon `ncdim4` object longitude of site
##' @param start_date start time of simulation
##' @param end_date end time of simulation
##' @param begins deprecated; use `start_date` instead
##' @param ends deprecated; use `end_date` instead
##' @param out deprecated; use `var_list` instead
##' 
##' @return a list of `ncdim4` objects
##' 
put_E_values <-
  function(yr,
           nc_var,
           var_list,
           lat,
           lon,
           start_date,
           end_date,
           begins,
           ends,
           out) {
    
  if(!missing(begins)) {
    warning("`begins` is deprecated, using `start_date` instead")
    start_date <- begins
  }
  if(!missing(ends)) {
    warning("`ends` is deprecated, using `end_date` instead")
    end_date <- ends
  }
  if(!missing(out)) {
    warning("`out` is deprecated, using `var_list` instead")
    var_list <- out
  }

  # Extract the PFT names and numbers for all PFTs
  pfts <- var_list$PFT
  
  
  # ----- fill list
  
  ##### setup output time and time bounds
  ## Create a date vector that contains each month of the model run (e.g.
  ## "2001-07-01" "2001-08-01" "2001-09-01"....) and which is the correct length
  ## for each full or partial year
  output_date_vector <-
    seq(
      lubridate::ymd(start_date),
      # an E file is only written if a month is completed.
      # E.g. start_date=2004-07-01, end_date=2004-08-31 will result in one E file for 2004-07
      lubridate::floor_date(lubridate::ymd(end_date), "month") - lubridate::days(1), 
      by = "month"
    )
  ## Create a vector of the number of days in each month by year (e.g. 31 31 30
  ## 31 30 31)
  num_days_per_month <- lubridate::days_in_month(output_date_vector)
  ## Update num_days_per_month and output_date_vector if model run did not start
  ## on the first day of a month e.g. "2001-07-15" "2001-08-01", 17 31
  if (lubridate::yday(start_date) != lubridate::yday(output_date_vector[1])) {
    temp <-
      num_days_per_month[1] - ((
        lubridate::yday(start_date) - lubridate::yday(output_date_vector[1])
      ))
    num_days_per_month[1] <- temp
    output_date_vector[1] <- start_date
  }
  ## Create a vector of output month julian dates (e.g. 196 213 244 274 305 335)
  jdates <- lubridate::yday(output_date_vector)
  ## Create a 0 index dtime variable 
  dtvals <- jdates - 1 # convert to 0 index
  ## Create monthly time bounds to populate dtime_bounds variable
  bounds <- array(data = NA, dim = c(length(dtvals), 2))
  bounds[, 1] <- dtvals 
  bounds[, 2] <- bounds[, 1] + num_days_per_month 
  # create time bounds for each timestep in t, t+1; t+1, t+2... format
  bounds <- round(bounds, 4) 
  
  t <-
    ncdf4::ncdim_def(
      name = "dtime",
      units = paste0("days since ", yr, "-01-01 00:00:00"),
      vals = dtvals,
      calendar = "standard",
      unlim = TRUE
    )
  time_interval <-
    ncdf4::ncdim_def(
      name = "hist_interval",
      longname = "history time interval endpoint dimensions",
      vals = 1:2,
      units = ""
    )
  p <-
    ncdf4::ncdim_def(
      name = "pft",
      units = "unitless",
      vals = pfts,
      longname = "Plant Functional Type",
      unlim = TRUE
    )
  
  # NOTE : the order of dimensions is going to be important for read.output.
  # This was the fist case of reading pft-specific outputs at the time, but
  # checking base/utils/data/standard_vars.csv "pft" should come after "time" as
  # a dimension e.g. when NEE is pft-specific for some model output it will be
  # the 4th dimension
  # lon / lat / time / pft 
  # From read.output's perspective, dimension of pft will be the same for NEE
  # there and DBH here

  # NOTE: the order of variables in `evars` MUST match the order in `var_list`
  # output by read_E_files.  Some day the whole model2netcdf.ED2 function should
  # probably be re-written to combine the read_*_files and put_*_values
  # functions to make this harder to accidentally screw up.
 evars <- list(
   ncdf4::ncvar_def(
     "AGB_PFT", #original ED2 name: AGB_CO
     units = "kgC m-2",
     dim = list(lon, lat, t, p),
     missval = -999,
     longname = "Above ground biomass by PFT"
   ),
   ncdf4::ncvar_def(
     "BSEEDS", #original ED2 name: BSEEDS_CO
     units = "kgC m-2",
     dim = list(lon, lat, t, p),
     missval = -999,
     longname = "Seed biomass by PFT"
   ),
    ncdf4::ncvar_def(
      "DBH", #original ED2 name: DBH
      units = "cm",
      dim = list(lon, lat, t, p),
      missval = -999,
      longname = "Diameter at breast height by PFT"
    ),
    ncdf4::ncvar_def(
      "DDBH", #original ED2 name: DDBH_DT
      units = "cm yr-1",
      dim = list(lon, lat, t, p),
      missval = -999,
      longname = "Rate of change in dbh by PFT"
    ),
   ncdf4::ncvar_def(
     "NPP_PFT", #original ED2 name: MMEAN_NPPDAILY_CO
     units = "KgC m-2 s-1",
     dim = list(lon, lat, t, p),
     missval = -999,
     longname = "Net primary productivity by PFT"
   ),
    ncdf4::ncvar_def(
      "TRANSP_PFT", #original ED2 name: MMEAN_TRANSP_CO
      units = "kg m-2 s-1",
      dim = list(lon, lat, t, p),
      missval = -999,
      longname = "Leaf transpiration by PFT"
    ),
   ncdf4::ncvar_def(
     "DENS", #original ED2 name: NPLANT
     units = "plant m-2",
     dim = list(lon, lat, t, p),
     missval = -999,
     longname = "Plant density by PFT"
   ),

    # longname of this variable will be parsed by read.output
    # so that read.output has a way of accessing PFT names
    ncdf4::ncvar_def(
      "PFT",
      units = "",
      dim = list(p),
      longname = paste(names(pfts), collapse = ",")
    ),
    ncdf4::ncvar_def(
      name = "dtime_bounds",
      units = "",
      longname = "monthly history time interval endpoints",
      dim = list(time_interval, dtime = t),
      prec = "double"
    )
 )
 #TODO: assure that nc_var and var_list are of same length and same order?
 nc_var <- append(nc_var, evars)
 var_list <- append(var_list, list(dtime_bounds = c(bounds)))
 
 return(list(nc_var = nc_var, out = var_list))
 
} # put_E_values




##' Read "S" files output by ED2
##' 
##' S-file contents are not written to standard netcdfs but are used by
##' read_restart from SDA's perspective it doesn't make sense to write and read
##' to ncdfs because ED restarts from history files
##'
##' @param sfile history file name e.g. "history-S-1961-01-01-000000-g01.h5"
##' @param outdir path to run outdir, where the -S- file is
##' @param pfts a named vector of PFT numbers where the names are PFT names
##' @param pecan_names string vector, pecan names of requested variables, e.g.
##'   c("AGB", "AbvGrndWood")
##' @param settings pecan settings object
##' @param ... currently unused
##'
##' @export
read_S_files <- function(sfile, outdir, pfts, pecan_names = NULL, settings = NULL, ...){
  
  PEcAn.logger::logger.info(paste0("*** Reading -S- file ***"))

  if (!is.null(settings)) {
    if(!inherits(settings, "Settings")) {
      PEcAn.logger::logger.error("`settings` should be a PEcAn 'Settings' object")
    }
    if(missing(pfts)) pfts <- extract_pfts(settings$pfts)
  }
  # commonly used vars
  if(is.null(pecan_names)) pecan_names <- c("AGB", "AbvGrndWood", "GWBI", "DBH")
  
  ed_varnames <- pecan_names
  
  # TODO: ed.var lookup function can also return deterministically related variables
  
  # translate pecan vars to ED vars
  trans_out    <- translate_vars_ed(ed_varnames)
  ed_varnames  <- trans_out$vars  # variables to read from history files
  ed_derivs    <- trans_out$expr  # derivations to obtain pecan standard variables
  add_vars     <- trans_out$addvars # these are the vars -if there are any- that won't be updated by analysis, but will be used in write_restart
  ed_units     <- trans_out$units # might use
  
  # List of vars to extract includes the requested one, plus others needed below 
  add_vars <- c(add_vars, "PFT", "AREA", "PACO_N", "NPLANT","DAGB_DT", "BDEAD", "DBH", 
                "BSTORAGE", "BALIVE", "BLEAF", "BROOT", "BSEEDS_CO", "BSAPWOODA", "BSAPWOODB")
  vars <- c(ed_varnames, add_vars) 
  
  # list to collect outputs
  ed.dat <- list()
  
  nc <- ncdf4::nc_open(file.path(outdir, sfile))
  allvars <- names(nc$var)
  if (!is.null(vars)) allvars <- allvars[ allvars %in% vars ]
  
  for (j in seq_along(allvars)) {
    ed.dat[[j]] <- list()
    ed.dat[[j]] <- ncdf4::ncvar_get(nc, allvars[j])
  }
  names(ed.dat) <- allvars
  
  ncdf4::nc_close(nc)
  
  
  # even if this is a SA run for soil, currently we are not reading any variable
  # that has a soil dimension. "soil" will be passed to read.output as pft.name
  # from upstream, when it's not part of the attribute it will read the sum
  soil.check <- grepl("soil", names(pfts))
  if(any(soil.check)){
    # for now keep soil out
    #TODO: print a message??
    pfts <- pfts[!(soil.check)]
  }
  
  out <- list()
  for (varname in pecan_names) {
    out[[varname]] <- array(NA, length(pfts))
  }
  
  
  # Get cohort-level variables 
  pft        <- ed.dat$PFT
  plant_dens <- ed.dat$NPLANT  # Cohort stem density -- plant/m2
  dbh        <- ed.dat$DBH # used in allometric eqns -- dbh
  
  # Get patch areas. In general patches aren't the same area, so this is needed to area-weight when averaging up to site level. Requires minor finnagling to convert patch-level AREA to a cohort-length variable. 
  patch_area  <- ed.dat$AREA    # unitless, a proportion of total site area  -- one entry per patch (always add up to 1)
  paco_n      <- ed.dat$PACO_N  # number of cohorts per patch
  
  patch_index <- rep(1:length(paco_n), times = paco_n)
  
  # read xml to extract allometric coeffs later
  configfile <- paste0(gsub("/out/", "/run/", outdir), "/config.xml")
  pars <- XML::xmlToList(XML::xmlParse(configfile))
  # remove non-pft sublists
  pars[names(pars) != "pft"] <- NULL
  # pass pft numbers as sublist names
  names(pars) <- pfts
  
  # Aggregate
  for (l in seq_along(pecan_names)) {
    
    variable <- PEcAn.utils::convert.expr(ed_derivs[l])  # convert
    expr <- variable$variable.eqn$expression
    
    sapply(variable$variable.eqn$variables, function(x) assign(x, ed.dat[[x]], envir = .GlobalEnv))
    tmp.var <- eval(parse(text = expr)) # parse
    
    if (ed_units[l] %in% c("kg/m2")) { # does this always mean this is a patch-level variable w/o per-pft values?
      out[[pecan_names[l]]] <- NA
      out[[pecan_names[l]]] <- sum(tmp.var * patch_area, na.rm = TRUE)
      
    } else {# per-pft vars
      for(k in seq_len(length(pfts))) {
        ind <- (pft == pfts[k])
        
        if (any(ind)) {
          # check for different variables/units?
          if (pecan_names[l] == "GWBI") {
            # use allometric equations to calculate GWBI from DDBH_DT
            ddbh_dt <- tmp.var
            ddbh_dt[!ind] <- 0
            dagb_dt <- ed.dat$DAGB_DT
            dagb_dt[!ind] <- 0
            
            # get b1Bl/b2Bl/dbh_adult from xml
            # these are in order so you can use k, but you can also extract by pft
            small   <- dbh <= as.numeric(pars[[k]]$dbh_adult)
            ddbh_dt[small]  <- as.numeric(pars[[k]]$b1Bl_small) / 2 * ddbh_dt[small]  ^ as.numeric(pars[[k]]$b2Bl_small)
            ddbh_dt[!small] <- as.numeric(pars[[k]]$b1Bl_large) / 2 * ddbh_dt[!small] ^ as.numeric(pars[[k]]$b2Bl_large)
            gwbi_ch <- dagb_dt - ddbh_dt
            #     kgC/m2/yr = kgC/plant/yr  *   plant/m2  
            plant2cohort <- gwbi_ch * plant_dens
            cohort2patch <- tapply(plant2cohort, list("patch" = patch_index), sum, na.rm = TRUE)
            out[[pecan_names[l]]][k] <- sum(cohort2patch * patch_area, na.rm = TRUE)
            
          } else if (ed_units[l] %in% c("kgC/plant")) {
            pft.var <- tmp.var
            pft.var[!ind] <- 0
            #     kgC/m2 = kgC/plant  *   plant/m2  
            plant2cohort <- pft.var * plant_dens
            # sum cohorts to aggrete to patches
            cohort2patch <- tapply(plant2cohort, list("patch" = patch_index), sum, na.rm = TRUE)
            # scale up to site-level 
            out[[pecan_names[l]]][k] <- sum(cohort2patch*patch_area, na.rm = TRUE)
            
          }
        }  #any(ind)-if SHOULD THERE BE AN ELSE? DOES ED2 EVER DRIVES SOME PFTs TO EXTINCTION? 
      } #k-loop
      
    }# per-pft or not
  } #l-loop
  
  
  # pass everything, unaggregated
  out$restart <- ed.dat
  
  
  return(out)
  
} # read_S_files


#' Extract pft numbers from settings$pfts
#' 
#' A helper function to extract a named vector of pft numbers from
#' `settings$pfts`.  Will use pft numbers in `settings` if they exist, otherwise
#' it'll match using the `pftmapping` dataset
#'
#' @param pfts settings$pfts
#'
#' @return named numeric vector
#'
extract_pfts <- function(pfts) {
  
  get_pft_num <- function(x) {
    pftmapping <- PEcAn.ED2::pftmapping
    pft_number <- x[["ed2_pft_number"]]
    pft_name <- x[["name"]]
    if(!is.null(pft_number)) {
      pft_number <- as.numeric(pft_number)
      if (!is.finite(pft_number)) {
        PEcAn.logger::logger.severe(
          "ED2 PFT number present but not parseable as number. Value was ",
          pft_number
        )
      }
    } else {
      pft_number <- pftmapping$ED[pftmapping$PEcAn == pft_name]
    }
    
    as.integer(pft_number)
  }
  
  # apply to all pfts in list
  pfts_out <- sapply(pfts, get_pft_num)
  names(pfts_out) <- pfts %>% sapply(`[[`, "name")
  
  #return named numeric vector:
  pfts_out
}


# A version of ncvar_put that returns the varid in warning messages
var_put <- function(nc, varid, vals, start = NA, count = NA) {
  output <- utils::capture.output(
    ncdf4::ncvar_put(nc = nc, varid = varid, vals = vals, start = start, count = count)
  )
  if(length(output)!=0) {
    cat(paste0("With '", varid$name, "':"), output, "\n")
  }
}

##-------------------------------------------------------------------------------------------------#
### EOF
