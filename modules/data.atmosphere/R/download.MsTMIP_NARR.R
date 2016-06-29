##' Download and conver to CF NARR single grid point from MSTIMIP server using OPENDAP interface
##' @name download.MsTMIP_NARR
##' @title download.MsTMIP_NARR
##' @export
##' @param outfolder
##' @param start_date
##' @param end_date
##' @param lat
##' @param lon
##'
##' @author James Simkins
download.MsTMIP_NARR <- function(outfolder, start_date, end_date, site_id, lat.in, lon.in, overwrite=FALSE, verbose=FALSE, ...){  
  require(PEcAn.utils)
  require(lubridate)
  require(ncdf4)
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  start_year <- year(start_date)
  end_year   <- year(end_date)
  site_id = as.numeric(site_id)
  outfolder = paste0(outfolder,"_site_",paste0(site_id %/% 1000000000, "-", site_id %% 1000000000))

  lat.in = as.numeric(lat.in)
  lon.in = as.numeric(lon.in)
  lat_trunc = floor(4*(84-as.numeric(lat.in)))
  lon_trunc = floor(4*(as.numeric(lon.in)+170))
  dap_base ='http://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/mstmip_driver_na_qd_climate_'
  

  dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  
  ylist <- seq(start_year,end_year,by=1)
  rows = length(ylist)
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = "MsTMIP_NARR",
                        stringsAsFactors = FALSE)
  
  var = data.frame(DAP.name = c("air_2m","dswrf","dlwrf","wnd_10m","apcp","shum_2m","rhum_2m"),
                   CF.name = c("air_temperature","surface_downwelling_shortwave_flux_in_air","surface_downwelling_longwave_flux_in_air","wind_speed","precipitation_flux","specific_humidity","relative_humidity"),
                   units = c('Kelvin',"W/m2","W/m2","m/s","kg/m2/s","g/g","%")
  )

  
  for (i in 1:rows){
    year = ylist[i]    
    
    ntime = ifelse(year%%4 == 0,2923,2919)
    
    loc.file = file.path(outfolder,paste("MsTMIP_NARR",year,"nc",sep="."))
    
    ## Create dimensions
    lat <- ncdim_def(name='latitude', units='degree_north', vals=lat.in, create_dimvar=TRUE)
    lon <- ncdim_def(name='longitude', units='degree_east', vals=lon.in, create_dimvar=TRUE)
    time <- ncdim_def(name='time', units="sec", vals=(1:ntime)*10800, create_dimvar=TRUE, unlim=TRUE)
    dim=list(lat,lon,time)
    
    var.list = list()
    dat.list = list()
    
    DAPvar = c("air","dswrf","dlwrf","wnd","apcp","shum","rhum")
    
    ## get data off OpenDAP
    for(j in 1:nrow(var)){
      if (var$DAP.name[j] == "dswrf") {
        (dap_file = paste0("http://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1220/mstmip_driver_na_qd_dswrf_",year,"_v1.nc4"))
      }
      else {
        (dap_file = paste0(dap_base,var$DAP.name[j],"_",year,"_v1.nc4")) }
      dap = nc_open(dap_file)
      dat.list[[j]] = ncvar_get(dap,as.character(DAPvar[j]),c(lon_trunc,lat_trunc,1),c(1,1,ntime))
      var.list[[j]] = ncvar_def(name=as.character(var$CF.name[j]), units=as.character(var$units[j]), dim=dim, missval=-999, verbose=verbose)
      nc_close(dap)
      
    }
    
    ## change units of precip to kg/m2/s instead of 3 hour accumulated precip
    dat.list[[5]] = dat.list[[5]]/10800
    
    ## put data in new file
    loc <- nc_create(filename=loc.file, vars=var.list, verbose=verbose)
    for(j in 1:nrow(var)){
      ncvar_put(nc=loc, varid=as.character(var$CF.name[j]), vals=dat.list[[j]])
    }
    nc_close(loc)
    
    results$file[i] <- loc.file
    results$host[i] <- fqdn()
    results$startdate[i] <- paste0(year,"-01-01 00:00:00")
    results$enddate[i] <- paste0(year,"-12-31 23:59:59")
    results$mimetype[i] <- 'application/x-netcdf'
    results$formatname[i] <- 'CF Meteorology'
    
  }
  
  invisible(results)
}

