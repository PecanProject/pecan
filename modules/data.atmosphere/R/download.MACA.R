##' Download MACA CMIP5 outputs for a single grid point using OPeNDAP and convert to CF 
##' @name download.MACA
##' @title download.MACA
##' @export
##' @param outfolder
##' @param start_date
##' @param end_date
##' @param lat
##' @param lon
##' @param model , select which MACA model to run (options are CM3, ESM2M, ESM2G)
##' @param scenario , select which scenario to run (options are rcp26, rcp45, rcp60, rcp85)
##' @param ensemble_member , select which ensemble_member to initialize the run (options are r1i1p1, r3i1p1, r5i1p1)
##'
##' @author James Simkins
download.MACA <- function(outfolder, start_date, end_date, site_id, lat.in, lon.in, overwrite=FALSE, verbose=FALSE, model='IPSL-CM5A-LR', scenario='rcp85', ensemble_member='r1i1p1', ...){  
  require(PEcAn.utils)
  require(lubridate)
  require(ncdf4)
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  start_year <- year(start_date)
  end_year   <- year(end_date)
  site_id = as.numeric(site_id)
  model = paste0(model)
  scenario = paste0(scenario)
  ensemble_member = paste0(ensemble_member)
  outfolder = paste0(outfolder,"_site_",paste0(site_id %/% 1000000000, "-", site_id %% 1000000000))
  
  if (model == 'CCSM4'){
    ensemble_member = 'r6i1p1'
  }
  
  lat.in = as.numeric(lat.in)
  lat = lat.in - 25.063077926635742
  lat_MACA = round(lat*24)
  lon.in = as.numeric(lon.in)
  if (lon.in < 0){
    lon.in = 180 - lon.in}
  lon = lon.in - 235.22784423828125
  lon_MACA = round(lon*24)
  

  dap_base ='http://thredds.northwestknowledge.net:8080/thredds/dodsC/MACAV2'

  ylist <- seq(start_year,end_year,by=1)
  rows = length(ylist)
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = paste("MACA",model,scenario,ensemble_member,sep="."),#"MACA",
                        stringsAsFactors = FALSE)
  
  
  prevar = data.frame(DAP.name = c("tasmax","tasmin","rsds","uas","vas","huss","pr"), 
                      CF.name = c("air_temperature","air_temperature","surface_downwelling_shortwave_flux_in_air","eastward_wind","northward_wind","specific_humidity","precipitation"),
                      units = c('Kelvin','Kelvin',"W/m2","m/s","m/s","g/g","kg/m2/s")
                      )
  var = data.frame(DAP.name = c("air_temperature", "rsds","uas","vas","huss","pr"),
                   CF.name = c("air_temperature","surface_downwelling_shortwave_flux_in_air","eastward_wind","northward_wind","specific_humidity","precipitation_flux"),
                   units = c('Kelvin',"W/m2","m/s","m/s","g/g","kg/m2/s")
  )
  
  for (i in 1:rows){
    year = ylist[i]    
    ntime = (1825)
    
    met_start = 2006
    met_block = 5
    url_year = met_start + floor((year-met_start)/met_block)*met_block
    start_url = paste0(url_year)
    end_url = paste0(url_year+met_block-1)
    dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
    
    loc.file = file.path(outfolder,paste("MACA",model,scenario,ensemble_member,year,"nc",sep="."))
    
    ## Create dimensions
    lat <- ncdim_def(name='latitude', units='degree_north', vals=lat.in, create_dimvar=TRUE)
    lon <- ncdim_def(name='longitude', units='degree_east', vals=lon.in, create_dimvar=TRUE)
    time <- ncdim_def(name='time', units="sec", vals=(1:365)*86400, create_dimvar=TRUE, unlim=TRUE)
    dim=list(lat,lon,time)
    
    prevar.list = list()
    predat.list = list()
    
    ## get data off OpenDAP
    for(j in 1:length(prevar$DAP.name)){
      dap_end = paste0('/',model,'/macav2metdata_',prevar$DAP.name[j],'_',model,'_',ensemble_member,'_',scenario,'_',start_url,'_',end_url,'_CONUS_daily.nc')
      dap_file = paste0(dap_base,dap_end)
      dap = nc_open(dap_file)
      predat.list[[j]] = ncvar_get(dap,as.character(prevar$CF.name[j]),c(lon_MACA,lat_MACA,1),c(1,1,ntime))
      prevar.list[[j]] = ncvar_def(name=as.character(prevar$CF.name[j]), units=as.character(prevar$units[j]), dim=dim, missval=-9999.0, verbose=verbose)
      nc_close(dap)
    }
    
    #take average of temperature min and max
    predat.list[[1]] = (predat.list[[1]]+predat.list[[2]])/2
    #convert mm precipitation to precipitation flux
    predat.list[[7]] = (predat.list[[7]]/(24*3600))
    prevar.list[[7]]$name = "precipitation_flux"
    predat.list[[2]] <- NULL
    prevar.list[[2]] <- NULL
    dat.list = list()
    var.list = list()
    dat.list = predat.list
    var.list = prevar.list
    
    #read in a 5 year file, but only storing 1 year at a time, so this selects the particular year of the 5 year span that you want
    dat.list = as.data.frame(dat.list)
    if (year%%5 == 1){
      dat.list = dat.list[1:365,]
    }
    if (year%%5 == 2){
      dat.list = dat.list[365:730,]
    }
    if (year%%5 == 3){
      dat.list = dat.list[730:1095,]
    }
    if (year%%5 == 4){
      dat.list = dat.list[1095:1460,]
    }
    if (year%%5 == 0){
      dat.list = dat.list[1460:1824,]
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

#download.MACA('maca','2006-01-01 00:00:00','2006-12-31 23:59:59',5,45,-90)

