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

site<-query.site(853,bety$con) # I have to include a site ID or be faced with a time zone error that betsy doesn't get but I do. On hers it translates it into UT
observations<-load_data(data.path = File_Path, format= File_format, time.row = File_format$time.row, site = site)

#### Read in Modle Run Data ####

#out <- read.output(1000010823, "/fs/data2/output/PEcAn_1000000993/out/1000333705" ,2004,2004,c("time","NEE"))

model_vars<-c("time", "NEE")
model <- as.data.frame(read.output(1000473575, "/projectnb/dietzelab/pecan.data/output/tmccabe/1000003037/out/1000473575" ,2000,2007,model_vars))
#vars.used.index <- which(format$vars$pecan_name %in% names(model)[!names(model) == "time"])
origin<-strptime(min(model$time), format ="%Y")
model$posix <- as.POSIXct(model$time*86400,origin= origin, tz="UTC")

#var <- filter(format$vars, variable_id == as.data.frame(model_vars))[, "pecan_name"]

#### Align Model and data ####
var<-as.character(c("NEE"))


aligned_data<-align_data(model.calc = model, obvs.calc= observations, var =var , start_year = "2004", end_year = "2009", align_method = "mean_over_larger_timestep")

plot(aligned_data$posix, aligned_data$NEE.m)


