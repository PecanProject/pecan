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

#######################################################
#                                                     #
#######################################################
# Choose meteorology data set. 

met <- "NARR"

#######################################################
# Set start and end dates (when possible otherwise NA)

start_date <- 1979 
end_date   <- 2014


#######################################################
# Location

# Set site id number when possible (not possible for NARR)
# RHIN: 1000000003
# ORNL: 854
site.id <- NA # site.id

# Set exrtraction site id when possible
newsite <- NA # extraction site id

regional = TRUE

if(regional==TRUE){
  site.id <- NA 
  extract <- TRUE 
}

#######################################################
# Download raw data? If not, specify raw.id
# NARR raw.id on geo = 285
# RHIN = 1000000059
# ORNL = 1000000060

raw    <- TRUE
raw.id <- 285

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

model <- ""

#######################################################
# Run met workflow

source("~/pecan/modules/data.atmosphere/inst/scripts/met.workflow.gen.R")