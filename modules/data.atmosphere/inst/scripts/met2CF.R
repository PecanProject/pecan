# met2CF converts met data to standard netcdf CF format
# requires an open database connection dbcon
# required steps:
# 1) query database using dbfile.input.check
# check to see if file exists as an input in dbfile
dbfile.input.check(siteid, startdate, enddate, mimetype, formatname, con, hostname=Sys.info()[['nodename']])
#   a) query input table select * where input.id = id
#     i) check to make sure there is only 1 match
#   b) query dbfiles table to get all rows that match the id
#     i) check should give back >0 entries, otherwise "File does not exist!"
#     ii) check that file exists on my server. If not, "File exists, but not on this server!"
# 2) check that file name is valid format
#   a) check that there is only 1 input ID
# 3) DO CONVERSIONS
  fldlw            <- Rgl         # incident longwave radiation W m-2
  fldsw            <- Rg - Rgl    # incident shortwave radiation W m-2
 # rainfall_flux   <-             # sum of rainfall kg m-2 s-1
 # snowfall_flux   <-             # sum of snowfall kg m-2 s-1
   ps              <- PRESS       # pressure at surface Pa
 # q               <-             # specific humidity kg kg-1. Requires vapor pressure and total pressure also.
  air_temperature  <- TA + 273.15 # 2 meter near surface air temp K
  wind             <- WS          # wind speed with vert. coord at h=10m m s-1
    
# 4) pass 1 file output to 1 output folder
#   a) generate file name, hardcoded: assign mime type, formate name, 
#      connection, hostname is localhost

  # inster into db as input
dbfile.input.insert <- function(filename, siteid, startdate, enddate, mimetype, formatname, con, hostname=Sys.info()[['nodename']]) {
    
