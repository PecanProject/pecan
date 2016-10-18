##' Download GFDL CMIP5 outputs for a single grid point using OPeNDAP and convert to CF 
##' @name download.GFDL
##' @title download.GFDL
##' @export
##' @param outfolder
##' @param start_date
##' @param end_date
##' @param lat
##' @param lon
##' @param model , select which GFDL model to run (options are CM3, ESM2M, ESM2G)
##' @param scenario , select which scenario to run (options are rcp26, rcp45, rcp60, rcp85)
##' @param ensemble_member , select which ensemble_member to initialize the run (options are r1i1p1, r3i1p1, r5i1p1)
##'
##' @author James Simkins
download.GFDL <- function(outfolder, start_date, end_date, site_id, lat.in, lon.in, 
                          overwrite = FALSE, verbose = FALSE, 
                          model = "CM3", scenario = "rcp45", ensemble_member = "r1i1p1", ...) {
  library(PEcAn.utils)
  library(ncdf4)
  
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date   <- as.POSIXlt(end_date, tz = "UTC")
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)
  site_id    <- as.numeric(site_id)
  model      <- paste0(model)
  scenario   <- paste0(scenario)
  ensemble_member <- paste0(ensemble_member)
  outfolder  <- paste0(outfolder, "_site_", paste0(site_id%/%1e+09, "-", site_id%%1e+09))
  
  lat.in     <- as.numeric(lat.in)
  lat_floor  <- floor(lat.in)
  lon.in     <- as.numeric(lon.in)
  lon_floor  <- floor(lon.in)
  if (lon_floor < 0) {
    lon_floor <- 360 + lon_floor
  }
  lat_GFDL <- lat_floor * (0.5) + 45
  lat_GFDL <- floor(lat_GFDL) + 1
  lon_GFDL <- lon_floor/2.5
  lon_GFDL <- floor(lon_GFDL) + 1
  
  start    <- as.Date(start_date, format = "Y%m%d")
  start    <- gsub("-", "", start)
  end      <- as.Date(end_date, format = "Y%m%d")
  end      <- gsub("-", "", end)
  dap_base <- "http://nomads.gfdl.noaa.gov:9192/opendap/CMIP5/output1/NOAA-GFDL/GFDL"
  
  dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  
  ylist <- seq(start_year, end_year, by = 1)
  rows <- length(ylist)
  results <- data.frame(file = character(rows), 
                        host = character(rows), 
                        mimetype = character(rows), 
                        formatname = character(rows), 
                        startdate = character(rows), 
                        enddate = character(rows), 
                        dbfile.name = paste("GFDL", model, scenario, ensemble_member, sep = "."),   #'GFDL',
                        stringsAsFactors = FALSE)
  var <- data.frame(DAP.name = c("tas", "rlds", "ps", "rsds", "uas", "vas", "huss", "pr"), 
                    CF.name = c("air_temperature", "surface_downwelling_longwave_flux_in_air",
                                "air_pressure", "surface_downwelling_shortwave_flux_in_air", 
                                "eastward_wind", "northward_wind", "specific_humidity", "precipitation_flux"), 
                    units = c("Kelvin", "W/m2", "Pascal", "W/m2", "m/s", "m/s", "g/g", "kg/m2/s"))
  
  for (i in seq_len(rows)) {
    year <- ylist[i]
    ntime <- (14600)
    
    loc.file <- file.path(outfolder,
                          paste("GFDL", model, scenario, ensemble_member, year, "nc", sep = "."))
    
    met_start <- 2006
    met_block <- 5
    url_year  <- met_start + floor((year - met_start) / met_block) * met_block
    start_url <- paste0(url_year, "0101")
    end_url   <- paste0(url_year + met_block - 1, "1231")
    
    ## Create dimensions
    lat <- ncdim_def(name = "latitude", units = "degree_north", vals = lat.in, create_dimvar = TRUE)
    lon <- ncdim_def(name = "longitude", units = "degree_east", vals = lon.in, create_dimvar = TRUE)
    time <- ncdim_def(name = "time", units = "sec", vals = (1:2920) * 10800, 
                      create_dimvar = TRUE, unlim = TRUE)
    dim <- list(lat, lon, time)
    
    var.list <- list()
    dat.list <- list()
    
    ## get data off OpenDAP
    for (j in seq_len(nrow(var))) {
      dap_end <- paste0("-", model, "/", 
                        scenario, "/3hr/atmos/3hr/", 
                        ensemble_member, "/v20110601/", 
                        var$DAP.name[j], "/", 
                        var$DAP.name[j], "_3hr_GFDL-",
                        model, "_", 
                        scenario, "_", 
                        ensemble_member, "_", 
                        start_url, "00-", end_url, "23.nc")
      dap_file <- paste0(dap_base, dap_end)
      dap <- nc_open(dap_file)
      dat.list[[j]] <- ncvar_get(dap, as.character(var$DAP.name[j]), 
                                 c(lon_GFDL, lat_GFDL, 1), 
                                 c(1, 1, ntime))
      var.list[[j]] <- ncvar_def(name = as.character(var$CF.name[j]), 
                                 units = as.character(var$units[j]), 
                                 dim = dim,
                                 missval = -999, 
                                 verbose = verbose)
      nc_close(dap)
      
    }
    dat.list <- as.data.frame(dat.list)
    if (year %% 5 == 1) {
      dat.list <- dat.list[1:2920, ]
    }
    if (year %% 5 == 2) {
      dat.list <- dat.list[2920:5839, ]
    }
    if (year %% 5 == 3) {
      dat.list <- dat.list[5840:8759, ]
    }
    if (year %% 5 == 4) {
      dat.list <- dat.list[8760:11679, ]
    }
    if (year %% 5 == 0) {
      dat.list <- dat.list[11680:14599, ]
    }
    
    ## put data in new file
    loc <- nc_create(filename = loc.file, vars = var.list, verbose = verbose)
    for (j in seq_len(nrow(var))) {
      ncvar_put(nc = loc, varid = as.character(var$CF.name[j]), vals = dat.list[[j]])
    }
    nc_close(loc)
    
    results$file[i]       <- loc.file
    results$host[i]       <- fqdn()
    results$startdate[i]  <- paste0(year, "-01-01 00:00:00")
    results$enddate[i]    <- paste0(year, "-12-31 23:59:59")
    results$mimetype[i]   <- "application/x-netcdf"
    results$formatname[i] <- "CF Meteorology"
  }
  
  invisible(results)
} # download.GFDL
