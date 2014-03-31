## Get meteorology variables from Ameriflux L2 netCDF files and convert to netCDF CF format
met2CF <- function(...dbfiles, dbcon,siteID){
  #---------------- Load libraries. -----------------------------------------------------------------#
  require(PEcAn.all)
  require(RPostgreSQL)
  require(ncdf4)
  #--------------------------------------------------------------------------------------------------#  
  
  ### if reading ameriflux .nc file ###
  nc<-nc_open("/Users/josh/Downloads/AMF_USMOz_2004_L2_WG_V004.nc",write=TRUE)
  
  nc <- ncvar_rename(nc=nc,'WS','wind_speed')
  nc <- ncvar_rename(nc=nc,'TA','air_temperature')
  nc <- ncvar_rename(nc=nc,'PRESS','air_pressure')
  nc <- ncvar_rename(nc=nc,'Rg','surface_downwelling_shortwave_flux')
  nc <- ncvar_rename(nc=nc,'Rgl','surface_downwelling_longwave_flux')
  nc <- ncvar_rename(nc=nc,'PREC','precipitation_flux')
  
  ## rename variable longname
  ## nothing in ncdf4 package seems to do this.
  ## may need to rebuild file instead of only renaming variables

  #open database connection to upload new .nc file
  dbparms <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password = "bety")
  con <- db.open(dbparms)  
  
  mimetype <- 'CF Meteorology'
  formatname <- 
    filename <-
    siteid <-
    startdate <-
    enddate <- 
    # insert into db as input
    dbfile.input.insert(filename, siteid, startdate, enddate, mimetype, formatname, con=con)

  ## end netCDF CF conversion ##
  
  