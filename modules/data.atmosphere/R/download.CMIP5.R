##' Download and convert to CMIP5 Models single grid point from CMIP5 server using OPENDAP interface
##' @name download.CMIP5
##' @title download.CMIP5
##' @export
##' @param outfolder
##' @param start_date
##' @param end_date
##' @param lat
##' @param lon
##'
##' @author James Simkins
download.CMIP5 <- function(outfolder, CMIP5_model, experiment, ensemble_member, start_date, end_date, site_id, lat.in, lon.in, overwrite=FALSE, verbose=FALSE, ...){  
  require(PEcAn.utils)
  require(lubridate)
  require(ncdf4)
  #start_url<- as.Date(as.character(start_date), "%Y-%m-%d %H:%M:%S")
  #start_url = format(start_url, format = "%Y%m%d%H%M%S")
  #end_url <- as.Date(as.character(end_date), "%Y-%m-%d %H:%M:%S")
  #end_url = format(end_url, format = "%Y%m%d%H%M%S")
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  start_year <- year(start_date)
  end_year   <- year(end_date)
  site_id = as.numeric(site_id)
  CMIP5_model = paste0(CMIP5_model)
  experiment = paste0(experiment)
  ensemble_member = paste0(ensemble_member)
  outfolder = paste0(outfolder,"_site_",paste0(site_id %/% 1000000000, "-", site_id %% 1000000000))

  #model = IPSL-CM5A-LR
  
  lat.in = as.numeric(lat.in)
  lon.in = as.numeric(lon.in)
  if (lat.in<0){
    lat.in = 90-lat.in*(-1)
  }
  lat_IPSL = trunc(.5278*(lat.in))
  if (lon.in<0){
    lon.in = lon.in*(-1)+180
  } #nc.dods?vas[0][0][0]
  lon_IPSL = trunc((.2667)*(lon.in))
  dap_IPSL =paste0('https://vesg.ipsl.upmc.fr/thredds/dodsC/GeoMIP/output/IPSL/',CMIP5_model,'/',experiment,'/3hr/atmos/3hr')
  # x = https://vesg.ipsl.upmc.fr/thredds/dodsC/GeoMIP/output/IPSL/IPSL-CM5A-LR/rcp45/3hr/atmos/3hr/r1i1p1/v20130428/vas/vas_3hr_IPSL-CM5A-LR_rcp45_r1i1p1_200601010300-205601010000.nc

  dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  
  ylist <- seq(start_year,end_year,by=1)
  rows = length(ylist)
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = "CMIP5",
                        stringsAsFactors = FALSE)
  
  var = data.frame(DAP.name = c("vas", "uas", "tas", "rlus", "rsds", "ps", "pr", "huss"),
                   CF.name = c("northward_wind", "eastward_wind", "air_temperature", "surface_upwelling_longwave_flux_in_air", "surface_downwelling_shortwave_flux_in_air","air_pressure", "precipitation_flux","specific_humidity"),
                   units = c('m/s', 'm/s', 'Kelvin', 'W/m2', 'W/m2', 'Pascal','kg/m2/s', 'g/g')
  )
  
  start_time = (as.numeric(start_year) - 2006)*2920
  end_time = (as.numeric(end_year) - 2005)*2920-1
  time_IPSL = paste0('[',start_time,':',end_time,']')
                     
  for (i in 1:rows){
    year = ylist[i]    
    ntime = (145999)

    
    loc.file = file.path(outfolder,paste("CMIP5_IPSL",start_time,end_time,"nc",sep="."))
    
    ## Create dimensions
    lat <- ncdim_def(name='latitude', units='degree_north', vals=lat.in, create_dimvar=TRUE)
    lon <- ncdim_def(name='longitude', units='degree_east', vals=lon.in, create_dimvar=TRUE)
    time <- ncdim_def(name='time', units="sec", vals=(1:ntime), create_dimvar=TRUE, unlim=TRUE)
    dim=list(lat,lon,time)
    
    ##no leap year in this dataset
    #nc.dods?vas[0][0][0]
    var.list = list()
    dat.list = list()
    ## get data off OpenDAP
    for(j in 1:nrow(var)){
      dap_end = paste0('/',ensemble_member,'/v20130428/',var$DAP.name[j],'/',var$DAP.name[j],'_3hr_',CMIP5_model,'_',experiment,'_',ensemble_member,'_200601010300-205601010000.nc.dods?')
      #dappp = 'https://vesg.ipsl.upmc.fr/thredds/dodsC/GeoMIP/output/IPSL/IPSL-CM5A-LR/rcp45/3hr/atmos/3hr/r1i1p1/v20130428/vas/vas_3hr_IPSL-CM5A-LR_rcp45_r1i1p1_200601010300-205601010000.nc.dods?'
      dap_file = paste0(dap_IPSL,dap_end,var$DAP.name[j],time_IPSL,'[',lat_IPSL,'][',lon_IPSL,']')
      dap = nc_open(dap_file)
      dat.list[[j]] = ncvar_get(dap,as.character(var$DAP.name[j]),c(lon_IPSL,lat_IPSL,1),c(1,1,ntime))
      var.list[[j]] = ncvar_def(name=as.character(var$CF.name[j]), units=as.character(var$units[j]), dim=dim, missval=-999, verbose=verbose)
      nc_close(dap)
    }
    
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

# https://pcmdi.llnl.gov/esgf-idp/openid/jsimkins2 is the OpenID used to access the files
# download.CMIP5('C:/Users/James Simkins/', IPSL-CM5A-LR, rcp45, r1i1p1, '2006-01-01 00:00:00', '2007-12-31 23:59:59', 2, 45, -90)

