library(PEcAn.all)
library(PEcAn.DB)
require(RPostgreSQL)
library(PEcAn.assim.batch)
library(PEcAn.benchmark)



####  Load Up Data to compare model output to ####
# Open up a connection to The Bety Database

settings <-list(database = list(bety = list(host = "psql-pecan.bu.edu", driver = "PostgreSQL", user = "bety", dbname = "bety", password = "bety")))
d <- settings$database$bety[c("dbname", "password", "host", "user")]
bety <- src_postgres(host = d$host, user = d$user, password = d$password, dbname = d$dbname)
settings$host$name <- "localhost"

# Query the database for the format your file is in. The defualt format ID "5000000002" is for csv files downloaded from the ameriflux website (aka BADM files).
# You could query for diffent kinds of formats that exist in bety or make your own. 
#File_format<-query.format.vars(bety = bety, format.id = 5000000002) #This for some reason works for Betsy but not for me. 
File_format<-query.format.vars(bety = bety, format.id = 5000000002) 
#File_format<-db.query(paste("SELECT variable_id,name,storage_type,column_number from formats_variables where format_id = 5000000002"),bety$con)

#Read in the data as a csv, txt or netCDF file  #TEST WITH NETCDF FILE

File_Path<-"/fs/data3/tmccabe/valicali/AMF_US-Dk3_BASE_HH_3-1.csv" #Replace with file path to your data

#data.path <- File_Path 
#format<-File_format 
#start_year = NA
#end_year = NA
#site = NA, 
#vars.used.index=NULL 
#time.row = File_format

# ARRRRRGH
db.query("UPDATE sites SET time_zone = 'EST' WHERE id = 853;", bety$con)
site<-query.site(853,bety$con) # I have to include a site ID or be faced with a time zone error that betsy doesn't get but I do. On hers it translates it into UT
observations<-load_data(data.path = File_Path, format= File_format, time.row = File_format$time.row, site = site, start_year = 2003, end_year = 2008)

#### Read in Modle Run Data ####

#out <- read.output(1000010823, "/fs/data2/output/PEcAn_1000000993/out/1000333705" ,2004,2004,c("time","NEE"))

origin<-"1997-01-01 00:00:00"
start_year = year(origin)
end_year = 2007

model_vars<-c("time", "NEE")
model <- as.data.frame(read.output(1000003037, "/fs/data2/output/PEcAn_1000003037/out/1000473575" ,start_year,end_year,model_vars))
#vars.used.index <- which(format$vars$pecan_name %in% names(model)[!names(model) == "time"])


# Make the model dates 
years <- start_year:end_year
seconds <- udunits2::ud.convert(model$time,"years","seconds")
Diff <- diff(model$time)
time_breaks = which(Diff < 0)

if(length(time_breaks) == 0 & length(years)>1){
  ## continuous time
  #model$year <- rep(years,each=round(365/median(Diff)))
  model$posix <- as.POSIXct(model$time*86400,origin,tz="UTC")
  model$year <- year(model$posix)
} else {
  N <- c(0,time_breaks, length(model$time))
  n <- diff(N)
  y <- c()
  for (i in seq_along(n)) {
    y <- c(y, rep(years[i], n[i]))
  }
  model$year <- y
  makeDate <- function(x){as.POSIXct(model$time[x]*86400,origin=paste0(model$year[x],"-01-01"),tz="UTC")}
  model$posix <- makeDate(seq_along(model$time))
}


#### Align Model and data ####
var<-as.character(c("NEE"))

model.calc = model
obvs.calc  = observations
var        = var 

align_method = "mean_over_larger_timestep"

dat = align_data(model.calc = model, obvs.calc= observations, var =var , start_year =start_year, end_year = end_year, align_method =align_method)

head(dat)

plot(dat$posix, dat$NEE.o, type="l")
lines(dat$posix,dat$NEE.m, col = "red")


