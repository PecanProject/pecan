rm(list = setdiff(ls(), lsf.str()))

# Database setup

driver   <- "PostgreSQL"
user     <- "bety"
dbname   <- "bety"
password <- "bety"
host     <- "psql-pecan.bu.edu"

# Select username, host and directory folder for data

username <- ""
raw.host <- "geo.bu.edu"
dir      <- "/projectnb/dietzelab/pecan.data/input/"

# raw.host <- "pecan2.bu.edu"
# dir      <- "/home/ecowdery/input/"

#######################################################
#                                                     #
#######################################################
# Choose meteorology data set. 

met <- "NARR"

#######################################################
# Set start and end dates (when possible otherwise NA)

start_year <- 2014 
end_year   <- 2014


#######################################################
# Location

# Set site id number when possible 
# RHIN: 1000000003
# ORNL: 854
# NARR: 1135
site.id <- 1135

# if(regional==TRUE){
#   site.id <- NA 
#   extract <- TRUE # 
# }

extract <- TRUE
newsite <- 340

#######################################################
# Download raw data? If not, specify raw.id
# NARR raw.id on geo = 285
# RHIN = 1000000059
# ORNL = 1000000060

raw    <- TRUE
raw.id <- NA

#######################################################
# Change to CF standards? If not, specify cf.id
# NARR cf.id on geo = 288

cf    <- TRUE
cf.id <- NA

#######################################################
# Permute data? If not, specify perm.id
# NARR perm.id on geo = 1000000023

perm    <- TRUE
perm.id <- NA

#######################################################
# Select model. Currently ED2 and SIPNET

model <- "ED2"

if(model == "ED2"){
  mod.formatname <- 'ed.met_driver_header files format'
  mod.mimetype <- 'text/plain'
}else if(model == "SIPNET"){
  mod.formatname <- 'Sipnet.climna'
  mod.mimetype <- 'text/csv'
}

#######################################################
# Run met workflow

# source("~/pecan/modules/data.atmosphere/inst/scripts/met.workflow.gen.R")