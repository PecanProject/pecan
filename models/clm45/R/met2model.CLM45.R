## R Code to convert NetCDF CF met files into NetCDF CLM met files.

##' met2model wrapper for CLM45
##'
##' @title met2model for CLM45
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param lst timezone offset to GMT in hours
##' @param lat,lon site coordinates
##' @param ... other arguments, currently ignored
##' @param overwrite should existing files be overwritten
##' @param verbose should the function be very verbosefor(year in start_year:end_year)
met2model.CLM45 <- function(in.path,in.prefix,outfolder,start_date, end_date, lst=0,lat,lon,..., overwrite=FALSE,verbose=FALSE){

  PEcAn.logger::logger.severe("NOT IMPLEMENTED")

  #General Structure- CLM Uses Netcdf so for now just need to rename vars.(Many not is CF standard. Need to Check that out)
  #Get Met file from inpath.
  #Loop over years (Open nc.file,rename vars,change dimensions as needed,close/save .nc file)
  #close
  #defining temporal dimension needs to be figured out. If we configure clm to use same tstep then we may not need to change dimensions
  
#   
#   #Process start and end dates
#   start_date<-as.POSIXlt(start.date,tz="UTC")
#   end_date<-as.POSIXlt(end.date,tz="UTC")
#
#   start_year <- year(start_date)
#   end_year <- year(end_date)
#
#   timestep.s<-86400 #Number of seconds in a day
#
#   ## Build met
#   met <- NULL
#   for(year in start_year:end_year){
#
#     met.file.y = paste(met.file,year,"nc",sep=".")
#
#     if(file.exists(met.file.y)){
#
#       ## Open netcdf file
#       nc=ncdf4::nc_open(met.file.y)
#
#
#       ## convert time to seconds
#       sec   <- nc$dim$time$vals
#       sec = PEcAn.utils::ud_convert(sec,unlist(strsplit(nc$dim$time$units," "))[1],"seconds")
#
#
#
#       ##build day and  year
#
#       dt <- PEcAn.utils::seconds_in_year(year) / length(sec)
#       tstep = round(timestep.s/dt) #time steps per day
#
#       diy <- PEcAn.utils::days_in_year(year)
#       doy <- rep(seq_len(diy), each=tstep)[1:length(sec)]
#

  ## extract variables. These need to be read in and converted to CLM standards

#   ncdf4::ncvar_rename(ncfile,varid="LONGXY")
#   ncdf4::ncvar_rename(ncfile,varid="LATIXY")
#   #     double ZBOT(time, lat, lon) ;
#   #     ZBOT:long_name = "observational height" ;
#   #     ZBOT:units = "m" ;
#   ZBOT = ncvar_rename(ncfile,"ZBOT","ZBOT")
#   #
#   #     double EDGEW(scalar) ;
#   #     EDGEW:long_name = "western edge in atmospheric data" ;
#   #     EDGEW:units = "degrees E" ;
#   EDGEW = ncvar_rename(ncfile,"EDGEW","EDGEW")
#
#   #     double EDGEE(scalar) ;
#   #     EDGEE:long_name = "eastern edge in atmospheric data" ;
#   #     EDGEE:units = "degrees E" ;
#   EDGEE = ncvar_rename(ncfile,"EDGEE","EDGEE")
#
#   #     double EDGES(scalar) ;
#   #     EDGES:long_name = "southern edge in atmospheric data" ;
#   #     EDGES:units = "degrees N" ;
#   EDGES = ncvar_rename(ncfile,"EDGES","EDGES")
#   #
#   #     double EDGEN(scalar) ;
#   #     EDGEN:long_name = "northern edge in atmospheric data" ;
#   #     EDGEN:units = "degrees N" ;
#   EDGEN = ncvar_rename(ncfile,"EDGEN","air_temperature")
#   #     double TBOT(time, lat, lon) ;
#   #     TBOT:long_name = "temperature at the lowest atm level (TBOT)" ;
#   #     TBOT:units = "K" ;
#   TBOT  = ncvar_rename(ncfile,"TBOT","specific_humidity")
#   #     double RH(time, lat, lon) ;
#   #     RH:long_name = "relative humidity at the lowest atm level (RH)" ;
#   #     relative_humidity
#   #     RH:units = "%" ;
#   RH    = ncvar_rename(ncfile,"RH","relative_humidity")
#   #     double WIND(time, lat, lon) ;
#   #     WIND:long_name = "wind at the lowest atm level (WIND)" ;
#   #     wind_speed
#   #     WIND:units = "m/s" ;
#   WIND  = ncvar_rename(ncfile,"WIND","wind_speed")
#   #     double FSDS(time, lat, lon) ;
#   #     FSDS:long_name = "incident solar (FSDS)" ;
#   #     FSDS:units = "W/m2" ;
#   FSDS  = ncvar_rename(ncfile,"FSDS","FSDS")
#   #     double FLDS(time, lat, lon) ;
#   #     FLDS:long_name = "incident longwave (FLDS)" ;
#   #     FLDS:units = "W/m2" ;
#   FLDS  = ncvar_rename(ncfile,"FLDS","")
#   #     double PSRF(time, lat, lon) ;
#   #     PSRF:long_name = "pressure at the lowest atm level (PSRF)" ;
#   #     PSRF:units = "Pa" ;
#   PSRF  = ncvar_rename(ncfile,"PSRF","air_pressure")
#   #     double PRECTmms(time, lat, lon) ;
#   #     PRECTmms:long_name = "precipitation (PRECTmms)" ;
#   #     PRECTmms:units = "mm/s" ;
#   PRECTmms =ncvar_rename(ncfile,"PRECTmmc","precipitation_flux")

 #nc_close(ncfiles)

#} ### end loop over met files

#print("Done with met2model.CLM4")

} ### end met2model.CLM4

