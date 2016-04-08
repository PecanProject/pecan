#-------------------------------------------------------------------------------
# Copyright (c) 2012 University of Illinois, NCSA.
# All rights reserved. This program and the accompanying materials
# are made available under the terms of the 
# University of Illinois/NCSA Open Source License
# which accompanies this distribution, and is available at
# http://opensource.ncsa.illinois.edu/license.html
#-------------------------------------------------------------------------------

##-------------------------------------------------------------------------------------------------#
##' Convert MAESPA output into the NACP Intercomparison format (ALMA using netCDF)
##' 
##' @name model2netcdf.MAESPA
##' @title Code to convert MAESPA's output into netCDF format
##'
##' @param outdir Location of MAESPA output
##' @param sitelat Latitude of the site
##' @param sitelon Longitude of the site
##' @param start_date Start time of the simulation
##' @param end_date End time of the simulation
##' @export
##'
##' @author Tony Gardella
model2netcdf.MAESPA <- function(outdir, sitelat, sitelon, start_date, end_date) {
  
  require(ncdf4)
  library(lubridate)
  
 
  ### Read in model output in MAESPA format
  # All FILENAMES:
  #hrflux.dat, Dayflx.dat, Maessarr.dat, watbal.dat,watbalday.dat,tutd.dat
  #watballay.dat,watupt.dat,watsoit.dat,testflx.dat,layflx.day
  
  con <- file(paste(outdir,"/","Dayflx.dat",sep = ""), "r", blocking = FALSE)
  Dayflx=readLines(con)
  colnames.index=grep(pattern = "Columns",x = Dayflx)
  colnames.raw=Dayflx[colnames.index]
  colnames.list= unlist(strsplit(colnames.raw,split= " "))
  remove=("")
  varnames=colnames.list[!colnames.list %in% remove]
  Dayflx.dat=read.table("Dayflx.dat",skip=colnames.index,col.names= varnames[-1])

  
  con <- file(paste(outdir,"/","watbalday.dat",sep = ""), "r", blocking = FALSE)
  watbalday=readLines("watbalday.dat")
  colnames.index=grep(pattern = "Columns",x = watbalday)
  colnames.raw=watbalday[colnames.index]
  colnames.list= unlist(strsplit(colnames.raw,split= " "))
  remove=("")
  varnames=colnames.list[!colnames.list %in% remove]
  watbalday.dat=read.table("watbalday.dat",skip=colnames.index,col.names= varnames[-1])
 
  
  ### Determine number of years and output timestep
  num.years <- round((length(unique(watbalday.dat$day)))/365)
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date<- as.POSIXlt(end_date, tz = "GMT")
  start_year <- year(start_date)
  end_year <- year(end_date)
  years <- start_year:end_year
  out.day <- max(watbalday.dat$day)/366
  timestep.s <- 86400/out.day
  
  
  for (y in years){
    if (file.exists(file.path(outdir, paste(y,"nc", sep=".")))) {
      next
    }
    print(paste("---- Processing year: ", y))  # turn on for debugging
    

    ## Setup outputs for netCDF file in appropriate units
    output <- list()
    output[[1]] <- (Dayflx.dat$totPs)       #(GPP) gross photosynthesis mol tree-1 d-1
    output[[2]] <- (Dayflx.dat$netPs)       #(NPP) photosynthesis net of foliar resp mol tree-1 d-1
    output[[3]] <- (watbalday.dat$et)         #(Tveg) modelled canopy transpiration   mm
    output[[4]] <- (watbalday.dat$qh)        #(Qh) Sensible heeat flux MJ m-2 day-1
    output[[5]] <- (watbalday.dat$qe)        #(Qle)latent Heat flux MJ m-2 day-1
  
    #******************** Declare netCDF variables ********************#
    t <- ncdim_def(name = "time",
                   units = paste0("days since ", y, "-01-01 00:00:00"),
                   vals =  (Dayflx.dat$DOY),
                   calendar = "standard", unlim = TRUE)
    lat <- ncdim_def("lat", "degrees_east",
                     vals =  as.numeric(sitelat),
                     longname = "station_latitude") 
    lon <- ncdim_def("lon", "degrees_north",
                     vals = as.numeric(sitelon),
                     longname = "station_longitude")

for(i in 1:length(output)){
  if(length(output[[i]])==0) output[[i]] <- rep(-999,length(t$vals))
}

mstmipvar <- PEcAn.utils::mstmipvar
var <- list()
var[[1]]  <- mstmipvar("GPP", lat, lon, t, NA)
var[[2]]  <- mstmipvar("NPP", lat, lon, t, NA)
var[[3]]  <- mstmipvar("TVeg", lat, lon, t, NA)
var[[4]]  <- mstmipvar("Qh", lat, lon, t, NA)
var[[5]]  <- mstmipvar("Qle", lat, lon, t, NA)

#******************** Declar netCDF variables ********************#


### Output netCDF data
nc <- nc_create(file.path(outdir, paste(y,"nc", sep=".")), var)
varfile <- file(file.path(outdir, paste(y, "nc", "var", sep=".")), "w")
for(i in 1:length(var)){
  #print(i)
  ncvar_put(nc,var[[i]],output[[i]])  
  cat(paste(var[[i]]$name, var[[i]]$longname), file=varfile, sep="\n")
}
close(varfile)
nc_close(nc)
} ### End of year loop



} ### End of function
