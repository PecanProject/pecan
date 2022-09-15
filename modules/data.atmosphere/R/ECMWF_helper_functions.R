#' Helper functions for met2CF.ECMWF
#' 
#' mergenc_ECMWF writes 1 CF and 50 PF netCDF files by extracting data from GRIB2 files and filling NaNs at 3h position wherever data is unavailable. 
#'
#' @param lat latitude
#' @param lon longitude
#' @param outfolder Path to directory where nc files need to be saved.
#' @param overwrite Logical if files needs to be overwritten.
#' @param verbose Logical flag defining if output of function be extra verbose.
#'
#' @return list of dataframes
#' @export
#' 
#' \dontrun{
#' results <- mergenc_ECMWF(lat.in,
#'                          lon.in,
#'                          outfolder,
#'                          overwrite = overwrite)
#'
#' @author Swarnalee Mazumder
#' 
nanfill <- function(variable) { # nanfill function fills NaN at every 3h period from 144h to 360h (original step 6h)
  
  len6 <- variable
  len3nan <- rep(NA,length(len6)*2-1)
  len3nan[seq(1, length(len6)*2-1, 2)]  <- len6
  
  return(len3nan)
}

mergenc_ECMWF <- function(lat.in,
                          lon.in,
                          outfolder,
                          overwrite = FALSE,
                          verbose = TRUE) {
  
  if (!file.exists(outfolder)) {
    dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  }
  
  
  # Python script "download_ecmwf.py" to get latest forecast date and download forecast datasets
  script.path = file.path(system.file("ECMWF/download_ecmwf.py", package = "PEcAn.data.atmosphere"))
  reticulate::source_python(script.path)
  
  time <- 0
  stream <- "enfo"
  type <- c("cf", "pf")
  step_15day <- 360
  
  all_parameters <-  c("10u", "10v", "2t", "sp", "tp")
  
  date_latestdata <- ecmwflatest(time= time, step15= step_15day, stream= stream, type= type, params= all_parameters)
  
  latest_filedate <- strtoi(gsub("-", "", substr(as.character.Date(date_latestdata), 1, 10)))
  
  current_date <- strtoi(gsub("-", "", Sys.Date()))
  
  in_fname <- paste(latest_filedate, time, stream, sep = "_")
  
  script.path = file.path(system.file("ECMWF/met2CFutils.py", package = "PEcAn.data.atmosphere"))
  reticulate::source_python(script.path)
  
  fnames_3h <- all_filenames_3h(in_fname)
  fnames_6h <- all_filenames_6h(in_fname)
  
  ######### CF ######### 
  
  ##### 3h CF -> 0 - 141h at 3h timestep
  cf_3h <- combine_files_cf(fnames_3h, lat_in, lon_in)
  names(cf_3h) <- c("valid_time", "lon", "lat", "u10", "v10", "t2m", "sp", "tp")
  
  days_since <- paste("days since", cf_3h$valid_time$time$data, "00:00:00")
  cf_3h$valid_time <- cf_3h$valid_time$values
  
  ##### 3h CF -> 144h - 360h at 6h timestep
  cf_6h_nc <- combine_files_cf(fnames_6h, lat_in, lon_in)
  names(cf_6h) <- c("valid_time", "lon", "lat", "u10", "v10", "t2m", "sp", "tp")
  
  cf_6h$valid_time <- cf_6h$valid_time$values
  
  ### Final CF netCDF filename
  cf_nc_fname <- paste(latest_filedate, time, stream, "cf.nc", sep = "_")
  
  # values from cf_3h
  lat <- cf_3h$lat
  lon <- cf_3h$lon
  validtime <- seq(0, 360, 3)
  validtimeunits <- cf_3h$valid_time
  
  # create and write the netCDF file -- ncdf4 version
  # define dimensions
  londim <- ncdf4::ncdim_def("longitude","degrees_east",as.double(lon)) 
  latdim <- ncdf4::ncdim_def("latitude","degrees_north",as.double(lat)) 
  timedim <- ncdf4::ncdim_def("valid_time",validtimeunits,as.double(validtime))
  time <- ncdf4::ncdim_def("time", days_since, as.double(validtime))

  # define variables
  fillvalue <- NA
  
  cf_u10_3 <- cf_3h$u10
  cf_u10_6_nan <- nanfill(cf_6h$u10)
  cf_u10 <- c(cf_u10_3, cf_u10_6_nan)
  
  cf_v10_3 <- cf_3h$v10
  cf_v10_6_nan <- nanfill(cf_6h$v10)
  cf_v10 <- c(cf_v10_3, cf_v10_6_nan)
  
  cf_t2m_3 <- cf_3h$t2m
  cf_t2m_6_nan <- nanfill(cf_6h$t2m)
  cf_t2m <- c(cf_t2m_3, cf_t2m_6_nan)
  
  cf_sp_3 <- cf_3h$sp
  cf_sp_6_nan <- nanfill(cf_6h$tp)
  cf_sp <- c(cf_sp_3, cf_sp_6_nan)
  
  cf_tp_3 <- cf_3h$tp
  cf_tp_6_nan <- nanfill(cf_6h$tp)
  cf_tp <- c(cf_tp_3, cf_tp_6_nan)
  
  dlname <- "Eastward Component of Wind"
  u10.def <- ncdf4::ncvar_def("eastward_wind","m s**-1",list(timedim),fillvalue,dlname,prec="double")
  
  dlname <- "Northward Component of Wind"
  v10.def <- ncdf4::ncvar_def("northward_wind","m s**-1",list(timedim),fillvalue,dlname,prec="double")
  
  dlname <- "Near surface air temperature"
  t2m.def <- ncdf4::ncvar_def("air_temperature","K",list(timedim),fillvalue,dlname,prec="double")
  
  dlname <- "Surface pressure"
  sp.def <- ncdf4::ncvar_def("air_pressure","Pa",list(timedim),fillvalue,dlname,prec="double")
  
  dlname <- "Rainfall rate"
  tp.def <- ncdf4::ncvar_def("precipitation_flux","kg m**-2 s**-1",list(timedim),fillvalue,dlname,prec="double")
  
  if (!file.exists(cf_nc_fname) || overwrite) {
    
    tryCatch({
      
      # create netCDF file and put arrays
      ncout_cf <- ncdf4::nc_create(cf_nc_fname, list(u10.def, v10.def, t2m.def, sp.def, tp.def),force_v4=TRUE)
      
      # put variables in netCDF file
      ncdf4::ncvar_put(ncout_cf,u10.def, cf_u10, verbose = TRUE)
      ncdf4::ncvar_put(ncout_cf,v10.def, cf_v10, verbose = TRUE)
      ncdf4::ncvar_put(ncout_cf,t2m.def, cf_t2m, verbose = TRUE)
      ncdf4::ncvar_put(ncout_cf,sp.def, cf_sp, verbose = TRUE)
      ncdf4::ncvar_put(ncout_cf,tp.def, cf_tp, verbose = TRUE)
      
      # add global attributes
      ncdf4::ncatt_put(ncout_cf,0,"GRIB_edition",2)
      ncdf4::ncatt_put(ncout_cf,0,"Institution","ecmwf")
      ncdf4::ncatt_put(ncout_cf,0,"Source","European Centre for Medium-Range Weather Forecasts")
      history <- paste("PEcAn Project", date(Sys.Date()), sep=", ")
      ncdf4::ncatt_put(ncout_cf,0,"Conventions","CF-1.7")
      
      ncdf4::nc_close(ncout_cf) # writes ensemble wise pf files to disk
      
    },
    
    error = function(e) {
      PEcAn.logger::logger.severe("Something went wrong during the writing of the nc file.",
                                  conditionMessage(e))
    })
    
  } else {
    PEcAn.logger::logger.info(paste0(
      "The file ",
      flname,
      " already exists.  It was not overwritten."
    ))
  }
  
  rows    <- 51
  results <- data.frame(
    file = character(rows),
    host = character(rows),
    mimetype = character(rows),
    formatname = character(rows),
    startdate = character(rows),
    enddate = character(rows),
    dbfile.name = "ECMWF",
    stringsAsFactors = FALSE
  )
  
  start_date <- latest_filedate
  end_date <- strtoi(gsub("-", "", lubridate::ymd(start_date) + days(14)))
  
  results$file[1]       <-
    file.path(outfolder, cf_nc_fname)
  results$host[1]       <- PEcAn.remote::fqdn()
  results$startdate[1]  <- start_date
  results$enddate[1]    <- end_date
  results$mimetype[1]   <- "application/x-netcdf"
  results$formatname[1] <- "CF Meteorology"
  
  ######### Perturbed Forecast ######### 
  
  ##### 3h PF -> 0 - 141h at 3h timestep
  pf_3h_out_fname <- paste(latest_filedate, time, stream, "pf_3h", sep = "_")
  pf_3h <- combine_files_pf(fnames_3h, lat_in, lon_in)
  names(pf_3h) <- c("valid_time", "lon", "lat", "u10", "v10", "t2m", "sp", "tp")
  
  days_since <- paste("days since", pf_3h$valid_time$time$data, "00:00:00")
  pf_3h$valid_time <- pf_3h$valid_time$values
  
  ##### 6h PF -> 144h - 360h at 6h timestep
  pf_6h_out_fname <- paste(latest_filedate, time, stream, "pf_6h", sep = "_")
  pf_6h <- combine_files_pf(fnames_6h, lat_in, lon_in)
  names(pf_6h) <- c("valid_time", "lon", "lat", "u10", "v10", "t2m", "sp", "tp")
  
  pf_6h$valid_time <- pf_6h$valid_time$values
  
 
  ##### Creating ensemble wise netCDF files
  
  pf_u10_3 <- t(pf_3h$u10)
  pf_u10_6 <- t(pf_6h$u10)
  
  pf_v10_3 <- t(pf_3h$v10)
  pf_v10_6 <- t(pf_6h$v10)
  
  pf_t2m_3 <- t(pf_3h$t2m)
  pf_t2m_6 <- t(pf_6h$t2m)
  
  pf_sp_3 <- t(pf_3h$sp)
  pf_sp_6 <- t(pf_6h$sp)
  
  pf_tp_3 <- t(pf_3h$tp)
  pf_tp_6 <- t(pf_6h$tp)
  
  for (ens in 1:50){
    
    ncfname_pf <- paste(latest_filedate, time, stream, "pf_ens", ens, sep = "_")
    
    if (!file.exists(ncfname_pf) || overwrite) {
      
      tryCatch({
        
        ncfname_pf <- paste0(latest_filedate, time, stream, "pf_ens", ens, sep = "_")
        
        pf_u10 <- c(pf_u10_3[, ens], nanfill(u10_6[, ens]))
        pf_v10 <- c(pf_v10_3[, ens], nanfill(v10_6[, ens]))
        pf_t2m <- c(pf_t2m_3[, ens], nanfill(pf_t2m_6[, ens]))
        pf_sp  <- c(pf_sp_3[, ens], nanfill(pf_sp_6[, ens]))
        pf_tp  <- c(pf_tp_3[, ens], nanfill(pf_tp_6[, ens]))
        
        ncout_pf <- ncdf4::nc_create(ncfname_pf, list(u10.def, v10.def, t2m.def, sp.def, tp.def),force_v4=TRUE)
        
        ncdf4::ncvar_put(ncout_pf,u10.def, pf_u10, verbose = TRUE)
        ncdf4::ncvar_put(ncout_pf,v10.def, pf_v10, verbose = TRUE)
        ncdf4::ncvar_put(ncout_pf,t2m.def, pf_t2m, verbose = TRUE)
        ncdf4::ncvar_put(ncout_pf,sp.def, pf_sp, verbose = TRUE)
        ncdf4::ncvar_put(ncout_pf,tp.def, pf_tp, verbose = TRUE)
        
        # add global attributes
        ncdf4::ncatt_put(ncout_pf,0,"GRIB_edition",2)
        ncdf4::ncatt_put(ncout_pf,0,"Institution","ecmwf")
        ncdf4::ncatt_put(ncout_pf,0,"Source","European Centre for Medium-Range Weather Forecasts")
        history <- paste("PEcAn Project", date(Sys.Date()), sep=", ")
        ncdf4::ncatt_put(ncout_pf,0,"Conventions","CF-1.7")
        
        ncdf4::nc_close(ncout_pf) # writes ensemble wise pf files to disk
        
        row <- ens # final data frame has 1+50 rows pertaining to 1cf + 50pf files
        results$file[row+1]  <- file.path(outfolder, ncout_pf)
        results$host[row+1]       <- "PEcAn.remote::fqdn()"
        results$startdate[row+1]  <- start_date
        results$enddate[row+1]    <- end_date
        results$mimetype[row+1]   <- "application/x-netcdf"
        results$formatname[row+1] <- "CF Meteorology"
        
      },
      
      error = function(e) {
        PEcAn.logger::logger.severe("Something went wrong during the writing of the nc file.",
                                    conditionMessage(e))
      })
      
    } else {
      PEcAn.logger::logger.info(paste0(
        "The file ",
        flname,
        " already exists.  It was not overwritten."
      ))
    }
  }
  
  return(results)
}