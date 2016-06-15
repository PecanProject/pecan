##' Download GFDL CMIP5 outputs for a single grid point using OPeNDAP and convert to CF 
##' @name download.GFDL
##' @title download.GFDL
##' @export
##' @param outfolder
##' @param start_date
##' @param end_date
##' @param lat
##' @param lon
##'
##' @author James Simkins
download.GFDL <- function(outfolder, model, experiment, scenario, start_date, end_date, site_id, lat.in, lon.in, overwrite=FALSE, verbose=FALSE, ...){  
  require(PEcAn.utils)
  require(lubridate)
  require(ncdf4)
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  start_year <- year(start_date)
  end_year   <- year(end_date)
  site_id = as.numeric(site_id)
  model = paste0(model)
  experiment = paste0(experiment)
  scenario = paste0(scenario)
  outfolder = paste0(outfolder,"_site_",paste0(site_id %/% 1000000000, "-", site_id %% 1000000000))
  
  lat.in = as.numeric(lat.in)
  lat_floor = floor(lat.in)
  lon.in = as.numeric(lon.in)
  lon_floor = floor(lon.in)
  if (lon_floor < 0){
    lon_floor = lon_floor + 180
  }
  lat_GFDL = lat_floor*(.5) +45
  lat_GFDL = round(lat_GFDL)
  lon_GFDL = lon_floor/2.5 
  lon_GFDL = round(lon_GFDL)
  
  
  
  start = as.Date(start_date,format='Y%m%d')
  start = gsub("-", "", start)
  end = as.Date(end_date,format='Y%m%d')
  end = gsub("-", "", end)
  dap_base ='http://nomads.gfdl.noaa.gov:9192/opendap/CMIP5/output1/NOAA-GFDL/GFDL'
  
  
  dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  
  ylist <- seq(start_year,end_year,by=1)
  rows = length(ylist)
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = "GFDL",
                        stringsAsFactors = FALSE)
  
  var = data.frame(DAP.name = c("tas","rlds","ps","rsds","uas","vas","huss","pr"),
                   CF.name = c("air_temperature","surface_downwelling_longwave_flux_in_air","air_pressure","surface_downwelling_shortwave_flux_in_air","eastward_wind","northward_wind","specific_humidity","precipitation_flux"),
                   units = c('Kelvin',"W/m2","Pascal","W/m2","m/s","m/s","g/g","kg/m2/s")
  )
  #2920 values per year for 3 hourly 
  
  if (start >= 20060101 & start< 20101231){
    start_url = paste0(20060101)
    end_url = paste0(20101231)
  }
  if (start >= 20110101 & start< 20151231){
    start_url = paste0(20110101)
    end_url = paste0(20151231)
  }
  if (start >= 20160101 & start< 20201231){
    start_url = paste0(20160101)
    end_url = paste0(20201231)
  }
  if (start >= 20210101 & start< 20251231){
    start_url = paste0(20210101)
    end_url = paste0(20251231)
  }
  if (start >= 20260101 & start< 20301231){
    start_url = paste0(20260101)
    end_url = paste0(20301231)
  }
  if (start >= 20310101 & start< 20351231){
    start_url = paste0(20310101)
    end_url = paste0(20351231)
  }
  if (start >= 20360101 & start< 20401231){
    start_url = paste0(20360101)
    end_url = paste0(20401231)
  }
  if (start >= 20410101 & start< 20451231){
    start_url = paste0(20410101)
    end_url = paste0(20451231)
  }
  if (start >= 20460101 & start< 20501231){
    start_url = paste0(20460101)
    end_url = paste0(20501231)
  }
  if (start >= 20510101 & start< 20551231){
    start_url = paste0(20510101)
    end_url = paste0(20551231)
  }
  if (start >= 20560101 & start< 20601231){
    start_url = paste0(20560101)
    end_url = paste0(20601231)
  }
  if (start >= 20610101 & start< 20651231){
    start_url = paste0(20610101)
    end_url = paste0(20651231)
  }
  if (start >= 20660101 & start< 20701231){
    start_url = paste0(20660101)
    end_url = paste0(20701231)
  }
  if (start >= 20710101 & start< 20751231){
    start_url = paste0(20710101)
    end_url = paste0(20751231)
  }
  if (start >= 20760101 & start< 20801231){
    start_url = paste0(20760101)
    end_url = paste0(20801231)
  }
  if (start >= 20810101 & start< 20851231){
    start_url = paste0(20810101)
    end_url = paste0(20851231)
  }
  if (start >= 20860101 & start< 20901231){
    start_url = paste0(20860101)
    end_url = paste0(20901231)
  }
  if (start >= 20910101 & start< 20951231){
    start_url = paste0(20910101)
    end_url = paste0(20951231)
  }
  if (start >= 20960101 & start< 21001231){
    start_url = paste0(20960101)
    end_url = paste0(21001231)
  }
  
  if (start_year%%5 == 1){
    time_range <- c(1:2919)
  }
  if (start_year%%5 == 2){
    time_range = c(2920:5839)
  }
  if (start_year%%5 == 3){
    time_range = c(5840:8759)
  }
  if (start_year%%5 == 4){
    time_range = c(8760:11679)
  }
  if (start_year%%5 == 0){
    time_range = c(11679:14599)
  }

  for (i in 1:rows){
    year = ylist[i]    
    ntime = (14599)
    
    loc.file = file.path(outfolder,paste("GFDL",model,start_year,"nc",sep="."))
    
    ## Create dimensions
    lat <- ncdim_def(name='latitude', units='degree_north', vals=lat.in, create_dimvar=TRUE)
    lon <- ncdim_def(name='longitude', units='degree_east', vals=lon.in, create_dimvar=TRUE)
    time <- ncdim_def(name='time', units="sec", vals=(1:2920)*10800, create_dimvar=TRUE, unlim=TRUE)
    dim=list(lat,lon,time)
    
    var.list = list()
    dat.list = list()
    
    ## get data off OpenDAP
    for(j in 1:nrow(var)){
      dap_end = paste0('-',model,'/',experiment,'/3hr/atmos/3hr/',scenario,'/v20110601/',var$DAP.name[j],'/',var$DAP.name[j],'_3hr_GFDL-',model,'_',experiment,'_',scenario,'_',start_url,'00-',end_url,'23.nc')
      dap_file = paste0(dap_base,dap_end)
      dap = nc_open(dap_file)
      dat.list[[j]] = ncvar_get(dap,as.character(var$DAP.name[j]),c(lat_GFDL,lon_GFDL,1),c(1,1,ntime))
      var.list[[j]] = ncvar_def(name=as.character(var$CF.name[j]), units=as.character(var$units[j]), dim=dim, missval=-999, verbose=verbose)
      nc_close(dap)
      
    }
    
    # GFDL data is held in 5 year datasets, so now we need to subset the dataset for the year we want
    d = as.data.frame(dat.list)
    d = d[c(1,2,3,4,5,6,7,8)][time_range,]
    
    ## put data in new file
    loc <- nc_create(filename=loc.file, vars=var.list, verbose=verbose)
    for(j in 1:nrow(var)){
      ncvar_put(nc=loc, varid=as.character(var$CF.name[j]), vals=d[[j]])
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

#download.GFDL <- function(outfolder, model, experiment, scenario, start_date, end_date, site_id, lat.in, lon.in, overwrite=FALSE, verbose=FALSE, ...)
#download.GFDL('C:/Users/James Simkins/', 'ESM2M', 'rcp45', 'r1i1p1', '2006-01-01 00:00:00', '2007-12-31 23:59:59', 2, 45, -90)

