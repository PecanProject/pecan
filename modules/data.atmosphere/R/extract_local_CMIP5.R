##' Extract NLDAS from local download
##' Extract NLDAS meteorology for a point from a local download of the full grid
# -----------------------------------
# Description
# -----------------------------------
##' @author Christy Rollinson
##' @description This function extracts CMIP5 data from grids that have been downloaded and stored locally.
##'              Files are saved as a netCDF file in CF conventions at *DAILY* resolution.  Note: At this point
##'              in time, variables that are only available at a native monthly resolution will be repeated to
##'              give a pseudo-daily record (and can get dealt with in the downscaling workflow).  These files 
##'              are ready to be used in the general PEcAn workflow or fed into the downscaling workflow.
# ----------------------------------- 
# Parameters
# -----------------------------------
##' @param outfolder - directory where output files will be stored
##' @param in.path - path to the raw full grids
##' @param start_date - first day for which you want to extract met (yyyy-mm-dd)
##' @param end_date - last day for which you want to extract met (yyyy-mm-dd)
##' @param lat.in site latitude in decimal degrees
##' @param lon.in site longitude in decimal degrees
##' @param model which GCM to extract data from
##' @param scenario which experiment to pull (p1000, historical, ...)
##' @param ensemble_member which CMIP5 experiment ensemble member
##' @param date.origin (optional) specify the date of origin for timestamps in the files being read.  
##'                    If NULL defaults to 1850 for historical simulations (except MPI-ESM-P) and 
##'                    850 for p1000 simulations (plus MPI-ESM-P historical).  Format: YYYY-MM-DD
##' @param adjust.pr - adjustment factor fore precipitation when the extracted values seem off
##' @param overwrite logical. Download a fresh version even if a local file with the same name already exists?
##' @param verbose logical. to control printing of debug info
##' @param ... Other arguments, currently ignored
##' @export
##'
# -----------------------------------
extract.local.CMIP5 <- function(outfolder, in.path, start_date, end_date, lat.in, lon.in, 
                                model , scenario , ensemble_member = "r1i1p1", date.origin=NULL, adjust.pr=1,
                                overwrite = FALSE, verbose = FALSE, ...){
  
  # Some GCMs don't do leap year; we'll have to deal with this separately
  # no.leap <- c("bcc-csm1-1", "CCSM4")
  
  if(is.null(date.origin)){
    if(scenario == "p1000" | GCM=="MPI-ESM-P") { 
      date.origin=as.Date("850-01-01") 
    } else if(scenario == "historical" & GCM!="MPI-ESM-P") {
      date.origin=as.Date("1850-01-01")
    } else {
      # PEcAn.logger::logger.error("No date.origin specified and scenario not implemented yet")
      date.origin=as.Date("0001-01-01")
    }
  } 
  
  
  # Days per month
  dpm <- lubridate::days_in_month(1:12)

  # Date stuff
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)

  lat.in = as.numeric(lat.in)
  lon.in = as.numeric(lon.in)
  # dir.nldas="http://hydro1.sci.gsfc.nasa.gov/thredds/dodsC/NLDAS_FORA0125_H.002"
  dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  
  ylist <- seq(start_year,end_year,by=1)
  rows = length(ylist)
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = "NLDAS",
                        stringsAsFactors = FALSE
                        )
  
  # The table of var name conversion
  # psl; sfcWind; tasmax; tasmin; huss
  #"co2", "mole_fraction_of_carbon_dioxide_in_air", "1e-6"
  var <- data.frame(DAP.name = c("tas", "tasmax", "tasmin", "rlds", "ps", "rsds", "uas", "vas", "sfcWind", "ua", "va", "huss", "pr", "co2mass"), 
                    CF.name = c("air_temperature", "air_temperature_maximum", "air_temperature_minimum", 
                                "surface_downwelling_longwave_flux_in_air",
                                "air_pressure", "surface_downwelling_shortwave_flux_in_air", 
                                "eastward_wind", "northward_wind", "wind_speed", "eastward_wind", "northward_wind", 
                                "specific_humidity", "precipitation_flux", "mole_fraction_of_carbon_dioxide_in_air"), 
                    units = c("Kelvin", "Kelvin", "Kelvin", "W/m2", "Pascal", "W/m2", "m/s", "m/s", "m/s", "m/s", "m/s", "g/g", "kg/m2/s", "1e-6"))
                    
  # Some constants for converting CO2 if it's there                  
  co2.molmass <- 44.01 # g/mol https://en.wikipedia.org/wiki/Carbon_dioxide#Atmospheric_concentration
  atm.molmass <- 28.97 # g/mol https://en.wikipedia.org/wiki/Density_of_air
  atm.masstot <- 5.1480e18 # kg https://journals.ametsoc.org/doi/10.1175/JCLI-3299.1
  atm.mol <- atm.masstot/atm.molmass

  # Figuring out what we have daily for and what we only have monthly for
  path.day <- file.path(in.path, "day")
  path.mo <- file.path(in.path, "month")
  
  vars.gcm.day <- dir(path.day)
  vars.gcm.mo <- dir(path.mo) 
  # If our extraction bath is different from what we had, modify it
  if("atmos" %in% vars.gcm.day){
	  path.day <- file.path(in.path, "day", "atmos", "day", ensemble_member, "latest")
	  path.mo <- file.path(in.path, "mon", "atmos", "Amon", ensemble_member, "latest")

	  vars.gcm.day <- dir(path.day)
  	vars.gcm.mo <- dir(path.mo)  	
  }
  vars.gcm.mo <- vars.gcm.mo[!vars.gcm.mo %in% vars.gcm.day]
  
  vars.gcm <- c(vars.gcm.day, vars.gcm.mo)
  
  # Rewriting the dap name to get the closest variable that we have for the GCM (some only give uss stuff at sea level)
  if(!("huss" %in% vars.gcm)) var$DAP.name[var$DAP.name=="huss"] <- "hus"
  if(!("ps" %in% vars.gcm)) var$DAP.name[var$DAP.name=="ps"] <- "psl"

  # Making sure we're only trying to grab the variables we have (i.e. don't try sfcWind if we don't have it)
  var <- var[var$DAP.name %in% vars.gcm,]
  
  # Native CMIP5 file structure is organized by variable and then with multiple years per file
  # this means we need to do some funky things to get all variables for one year into a single file
  var$DAP.name <- as.character(var$DAP.name)
  
  files.var <- list()
  n.file=0
  for(v in var$DAP.name){
  	files.var[[v]] <- list()
    if(v %in% vars.gcm.day){
	    v.res="day"
      # Get a list of file names
      files.var[[v]] <- data.frame(file.name=dir(file.path(path.day, v))) 		
  	} else {
  	  v.res="month"
  	  files.var[[v]] <- data.frame(file.name=dir(file.path(path.mo, v)))
  	}
  	
	  # Set up an index to help us find out which file we'll need
    # files.var[[v]][["years"]] <- data.frame(first.date=NA, last.date=NA)
    for(i in 1:nrow(files.var[[v]])){
    	dt.str <- stringr::str_split(stringr::str_split(files.var[[v]][i,"file.name"], "_")[[1]][6], "-")[[1]]

    	# Don't bother storing this file if we don't want those years
    	if(v.res=="day"){
    	  files.var[[v]][i, "first.date"] <- as.Date(dt.str[1], format="%Y%m%d")
    	  files.var[[v]][i, "last.date" ] <- as.Date(substr(dt.str[2], 1, 8), format="%Y%m%d")
    	} else {
    	  # For monthly data, we can assume the first day of the month is day 1 of that month
    	  # dfirst <- lubridate::days_in_month(as.numeric(substr(dt.str[1], 5, 6)))
    	  files.var[[v]][i, "first.date"] <- as.Date(paste0(dt.str[1], 01), format="%Y%m%d")
    	  
    	  # For the last day, i wish we could assume it ends in December, but some models are 
    	  # jerks, so we should double check
    	  dlast <- lubridate::days_in_month(as.numeric(substr(dt.str[2], 5, 6)))
    	  files.var[[v]][i, "last.date" ] <- as.Date(paste0(substr(dt.str[2], 1, 6), dlast), format="%Y%m%d")
    	}

  	 } # End file loop
  	
  	# get rid of files outside of what we actually need
  	files.var[[v]] <- files.var[[v]][files.var[[v]]$first.date<=as.Date(end_date) & files.var[[v]]$last.date>=as.Date(start_date),]
  	# if(as.numeric(substr(yr.str[1], 1, 4)) > end_year | as.numeric(substr(yr.str[2], 1, 4))< start_year) next
		n.file=n.file+nrow(files.var[[v]])
  	
  } # end variable loop

  # Querying large netcdf files 1,000 times is slow.  So lets open the connection once and 
  # pull the full time series
  # Loop through using the files using the first variable; shoudl be tair & should be highest res avail
  # This will require quite a bit of memory, but it's doable
  dat.all <- list()
  dat.time <- seq(start_date, end_date, by="day")  # Everything should end up being a day
  
  print("- Extracting files: ")
  pb <- utils::txtProgressBar(min=1, max=n.file, style=3)
  pb.ind=1
  # Loop through each variable so that we don't have to open files more than once
  for(v in 1:nrow(var)){
    
    var.now <- var[v,"DAP.name"]
    # print(var.now)
    
    dat.all[[v]] <- vector() # initialize the layer
    # Figure out the temporal resolution of the variable
    v.res <- ifelse(var.now %in% vars.gcm.day, "day", "month")
    p.res <- ifelse(var.now %in% vars.gcm.day, path.day, path.mo)

    # Figure out what file we need
    # file.ind <- which(files.var[[var.now]][i])
    for(i in 1:nrow(files.var[[var.now]])){
      utils::setTxtProgressBar(pb, pb.ind)
      pb.ind=pb.ind+1
      f.now <- files.var[[var.now]][i,"file.name"]
      # print(f.now)
      
      # Open up the file
      ncT <- ncdf4::nc_open(file.path(p.res, var.now, f.now))
      
      # Extract our dimensions
      # Check to see if we need to extract lat/lon or not
      if(ncT$var[[var.now]]$ndims>1){
	      lat_bnd <- ncdf4::ncvar_get(ncT, "lat_bnds")
    	  lon_bnd <- ncdf4::ncvar_get(ncT, "lon_bnds")      	
      }
      nc.time <- ncdf4::ncvar_get(ncT, "time")

      if(v.res=="day"){
        date.leaps <- seq(files.var[[var.now]][i,"first.date"], files.var[[var.now]][i,"last.date"], by="day")
      } else {
        # if we're dealing with monthly data, start with the first of the month
        date.leaps <- seq(files.var[[var.now]][i,"first.date"], files.var[[var.now]][i,"last.date"], by="day")
      }
      # Figure out if we're missing leap dat
      no.leap <- ifelse(length(nc.time)!=length(date.leaps), TRUE, FALSE)
      
      # splt.ind <- ifelse(GCM %in% c("MPI-ESM-P"), 4, 3)
      # date.origin <- as.Date(stringr::str_split(ncT$dim$time$units, " ")[[1]][splt.ind])
      if(v.res == "day"){
        nc.date <- date.origin + nc.time
        
        nc.min <- as.Date(min(nc.date))
        # mean(diff(nc.date))
        date.ref <- files.var[[var.now]][i,"first.date"]+0.5 # Set a half-day offset to make centered
        
        # If things don't align with the specified origin, update it & try again
        if(nc.min != date.ref){
          date.off <- date.ref - nc.min # Figure out our date offset
          
          nc.date <- date.origin + nc.time + date.off 
        } 
      } else {
        dfirst <- lubridate::days_in_month(lubridate::month(files.var[[var.now]][i,"first.date"]))
        
        dates.mo <- seq.Date(files.var[[var.now]][i,"first.date"]+dfirst/2, files.var[[var.now]][i,"last.date"], by="month")
        
        if(length(dates.mo) == length(nc.time)){
          nc.date <- dates.mo
        } else {
          # I have no freaking clue what to do if things don't work out, so lets just go back to whatever we first tried
          date.off <- date.ref - nc.min # Figure out our date offset
          
          nc.date <- nc.date + date.off + 1
        }
      }

      
      # If we're missing leap year, lets adjust our date stamps so we can only pull what we need
      if(v.res=="day" & no.leap==TRUE){
        cells.bump <- which(lubridate::leap_year(lubridate::year(date.leaps)) & lubridate::month(date.leaps)==02 & lubridate::day(date.leaps)==29)
        for(j in 1:length(cells.bump)){
          nc.date[(cells.bump[j]-1):length(nc.date)] <- nc.date[(cells.bump[j]-1):length(nc.date)]+1
        }
      }
      
      # Find our time index
      if(v.res=="day"){
        time.ind <- which(nc.date>=as.Date(start_date) & nc.date<=as.Date(end_date)+0.5)
      } else {
        # date.ind <- rep(files.var[[var.now]][i,"first.date"]:files.var[[var.now]][i,"last.date"], each=12)
        time.ind <- which(nc.date>=as.Date(start_date) & nc.date<=as.Date(end_date)+0.5)
      }
      
      # Subset our dates & times to match our index
      nc.date <- nc.date[time.ind]
      date.leaps <- date.leaps[which(date.leaps>=as.Date(start_date) & date.leaps<=as.Date(end_date))]
      
      # Find the closest grid cell for our site (using harvard as a protoype)
      ind.lat <- which(lat_bnd[1,]<=lat.in & lat_bnd[2,]>=lat.in)
      if(max(lon.in)>=180){
        ind.lon <- which(lon_bnd[1,]>=lon.in & lon_bnd[2,]<=lon.in)
      } else {
        ind.lon <- which(lon_bnd[1,]<=180+lon.in & lon_bnd[2,]>=180+lon.in)
      }
      
      # Extract all of the available data
      if(var.now %in% c("hus", "ua", "va")){ # These have multiple strata; we only want 1
        plev <- ncdf4::ncvar_get(ncT, "plev")
        puse <- which(plev==max(plev)) # Get humidity at the place of highest pressure (closest to surface)
        dat.temp <- ncdf4::ncvar_get(ncT, var.now, c(ind.lon, ind.lat, puse, time.ind[1]), c(1,1,1,length(time.ind)))
        # If dat.list has missing values, try the next layer
        puse.orig <- puse
        while(is.na(mean(dat.temp))){
          if(puse.orig==1) { puse = puse + 1 } else { puse = puse -1 }
          dat.temp <- ncdf4::ncvar_get(ncT, var.now, c(ind.lon, ind.lat, puse, time.ind[1]), c(1,1,1,length(time.ind)))
        }
      } else {
        # Note that CO2 appears to be a global value
        if(ncT$var[[var.now]]$ndims==1){
	        dat.temp <- ncdf4::ncvar_get(ncT, var.now, c(time.ind[1]), c(length(time.ind)))
        } else {
	        dat.temp <- ncdf4::ncvar_get(ncT, var.now, c(ind.lon, ind.lat, time.ind[1]), c(1,1,length(time.ind)))	
        }        
      }
      
      # Add leap year and trick monthly into daily
      # Figure out if we're missing leap year
      if(v.res=="day" & no.leap==TRUE){
        cells.dup <- which(lubridate::leap_year(lubridate::year(date.leaps)) & lubridate::month(date.leaps)==02 & lubridate::day(date.leaps)==28)
        if(length(cells.dup)>0){
          for(j in 1:length(cells.dup)){
            dat.temp <- append(dat.temp, dat.temp[cells.dup[j]], cells.dup[j])
          }
        }
      }
      
      
      # If we have monthly data, lets trick it into being daily
      if(v.res == "month"){
        mo.ind <- rep(1:12, length.out=length(dat.temp))
        yr.ind <- lubridate::year(nc.date)
        dat.trick <- vector()
        for(j in 1:length(dat.temp)){
          if(lubridate::leap_year(yr.ind[j]) & mo.ind[j]==2){
            dat.trick <- c(dat.trick, rep(dat.temp[j], dpm[mo.ind[j]]+1)) 
          } else {
            dat.trick <- c(dat.trick, rep(dat.temp[j], dpm[mo.ind[j]])) 
          }
        }
        dat.temp <- dat.trick
      } # End leap day trick
      
      dat.all[[v]] <- append(dat.all[[v]], dat.temp, length(dat.all[[v]]))
      ncdf4::nc_close(ncT)    	
    } # End file loop
  } # End variable loop
    

  print("")
  print("- Writing to NetCDF: ")
  pb <- utils::txtProgressBar(min=1, max=rows, style=3)
  for (i in 1:rows){
    utils::setTxtProgressBar(pb, i)
    
    y.now = ylist[i]    
    yr.ind <- which(lubridate::year(dat.time)==y.now)
    
    
    dpm <- lubridate::days_in_month(1:12)
    if(lubridate::leap_year(y.now)) dpm[2] <- dpm[2] + 1 # make sure Feb has 29 days if we're dealing with a leap year
    
    # figure out how many days we're working with
    if(rows>1 & i!=1 & i!=rows){ # If we have multiple years and we're not in the first or last year, we're taking a whole year
      nday  = ifelse(lubridate::leap_year(y.now), 366, 365) # leap year or not; days per year
      day1 = 1
      day2 = nday
      days.use = day1:day2
    } else if(rows==1){
      # if we're working with only 1 year, lets only pull what we need to
      nday  = ifelse(lubridate::leap_year(y.now), 366, 365) # leap year or not; days per year
      day1 <- lubridate::yday(start_date)
      # Now we need to check whether we're ending on the right day
      day2 <- lubridate::yday(end_date)
      days.use = day1:day2
      nday=length(days.use) # Update nday
    } else if(i==1) {
      # If this is the first of many years, we only need to worry about the start date
      nday  = ifelse(lubridate::leap_year(y.now), 366, 365) # leap year or not; days per year
      day1 <- lubridate::yday(start_date)
      day2 = nday
      days.use = day1:day2
      nday=length(days.use) # Update nday
    } else if(i==rows) {
      # If this is the last of many years, we only need to worry about the start date
      nday  = ifelse(lubridate::leap_year(y.now), 366, 365) # leap year or not; days per year
      day1 = 1
      day2 <- lubridate::yday(end_date)
      days.use = day1:day2
      nday=length(days.use) # Update nday
    }
    ntime = nday # leap year or not; time slice (coerce to daily)
    
    loc.file <- file.path(outfolder, paste(model, scenario, ensemble_member, stringr::str_pad(y.now, width=4, side="left",  pad="0"), "nc", sep = "."))
    
    
    ## Create dimensions
    dim.lat <- ncdf4::ncdim_def(name='latitude', units='degree_north', vals=lat.in, create_dimvar=TRUE)
    dim.lon <- ncdf4::ncdim_def(name='longitude', units='degree_east', vals=lon.in, create_dimvar=TRUE)
    dim.time <- ncdf4::ncdim_def(name='time', units="sec", vals=seq((min(days.use)+1-1/24)*24*360, (max(days.use)+1-1/24)*24*360, length.out=ntime), create_dimvar=TRUE, unlim=TRUE)
    nc.dim=list(dim.lat,dim.lon,dim.time)
    
    
    # Defining our dimensions up front
    var.list = list()
    dat.list = list()

    for(j in 1:nrow(var)){
      var.list[[j]] = ncdf4::ncvar_def(name=as.character(var$CF.name[j]), units=as.character(var$units[j]), dim=nc.dim, missval=-999, verbose=verbose)
      dat.list[[j]] <- array(NA, dim=c(length(lat.in), length(lon.in), ntime)) # Go ahead and make the arrays
    }
    names(var.list) <- names(dat.list) <- var$CF.name
    
    # Loop through each variable in the order of everything else
    for(v in 1:nrow(var)){
	    	dat.list[[v]] <- dat.all[[v]][yr.ind]	
    } # End variable loop
    
    # Adjusting Preciptiation if necessary
    dat.list[["precipitation_flux"]] <- dat.list[["precipitation_flux"]]*adjust.pr
    
    if("mole_fraction_of_carbon_dioxide_in_air" %in% names(dat.list)){        
        co2.mol <- dat.list[["mole_fraction_of_carbon_dioxide_in_air"]]/co2.molmass # kg co2
        dat.list[["mole_fraction_of_carbon_dioxide_in_air"]] <- co2.mol/atm.mol*1e6 # kmol/kmol * 1e6 to be in CF units (ppm)
    }
    
    ## put data in new file
    loc <- ncdf4::nc_create(filename=loc.file, vars=var.list, verbose=verbose)
    for(j in 1:nrow(var)){
      ncdf4::ncvar_put(nc=loc, varid=as.character(var$CF.name[j]), vals=dat.list[[j]])
    }
    ncdf4::nc_close(loc)

    results$file[i] <- loc.file
    # results$host[i] <- fqdn()
    results$startdate[i]  <- paste0(as.Date(paste(y.now, day1, sep="-"), format = "%Y-%j"), " 00:00:00")
    results$enddate[i]    <- paste0(as.Date(paste(y.now, day2, sep="-"), format = "%Y-%j"), " 00:00:00")
    results$mimetype[i]   <- 'application/x-netcdf'
    results$formatname[i] <- 'CF Meteorology'
    
  } # End i loop (rows/years)
  
} # End function

