##################### OLD ##################################
# testing netCDF packages for oppening .nc files from GE375
# require(ncdf4)
# ncvar_get(nc="US-PFa.ED2.1998.nc")
# require(RNetCDF)
# nc<-open.nc(con="US-PFa.ED2.1998.nc",write=FALSE)
# require(ncdf)
# ncdf<-open.ncdf(con="US-PFa.ED2.1998.nc",write=FALSE)
# dat<-get.var.ncdf(ncdf,varid="Lwdown")

#when function was going to deal with database connection#
dbConnect("PostgreSQL",dbname="bety",user="bety")

#   query.inputs <-function(){
#     
#   }
#   
#   #generic function to query database from query.base.R
#   query.base()  

#query database
db.query(dbfiles,dbcon)

#   #open database connection
#   query.base.con <- function(settings,...){
#     invisible(db.open(settings$database))
#   }
# met2CF converts met data to standard netcdf CF format
# requires an open database connection dbcon
# required steps:
# 1) query database using dbfile.input.check
# check to see if file exists as an input in dbfile
dbfile.input.check(siteid, startdate, enddate, mimetype, formatname, con)
#   a) query input table select * where input.id = id
#     i) check to make sure there is only 1 match
#   b) query dbfiles table to get all rows that match the id
#     i) check should give back >0 entries, otherwise "File does not exist!"
#     ii) check that file exists on my server. If not, "File exists, but not on this server!"
# 2) check that file name is valid format
#   a) check that there is only 1 input ID
# 3) DO CONVERSIONS
#   fldlw            <- Rgl         # incident longwave radiation W m-2
#   fldsw            <- Rg          # incident shortwave radiation W m-2
#   # rainfall_flux   <-             # sum of rainfall kg m-2 s-1
#   # snowfall_flux   <-             # sum of snowfall kg m-2 s-1
#   precipitation_flux <- PREC     # rainfall or wintertime precip
#   wind_speed <- 
#     ps              <- PRESS       # pressure at surface Pa
#   # q               <-             # specific humidity kg kg-1. Requires vapor pressure and total pressure also.
#   air_temperature  <- TA + 273.15 # 2 meter near surface air temp K
#   wind             <- WS          # wind speed with vert. coord at h=10m m s-1

# 4) pass 1 file output to 1 output folder
#   a) generate file name, hardcoded: assign mime type, formate name, 
#      connection, hostname is localhost
# test conversions using one ameriflux file not in database

##################### END OLD ##############################

### if reading ameriflux .csv file ###
dat <- read.csv("AMF_USMOz_2004_L2_WG_V004.csv",skip=17,na.strings=c(-9999,-6999)) #example file
units <- dat[1,]
dat <- dat[-1:-2,]
dat$TA <- as.numeric(dat$TA)+273.15

vars <- c("WS","TA","PRESS","Rg","Rgl","PREC")
new.vars <- c("wind_speed","air_temperature","air_pressure",
              "surface_downwelling_shortwave_flux",
              "surface_downwelling_longwave_flux","precipitation_flux") #netCDF CF long variable names

names(dat)[names(dat) %in% vars] <- new.vars #replace column names with netCDF CF names

ncdim_def() #define netCDF dimensions for variables
ncvar_def(name="wind_speed",units="m s-1",dim) #define netCDF variables
nc_create("AMF_USMOz_L2_WG_02004.nc", vars=dat[new.vars]) #create netCDF file

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
### end reading .csv file ###
