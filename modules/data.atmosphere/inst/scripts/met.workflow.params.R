# Database setup
driver   <- "PostgreSQL"
user     <- "bety"
dbname   <- "bety"
password <- "bety"
host     <-  "psql-pecan.bu.edu"

#######################################################
# Choose data set. 
# Current choices: 
# NARR
# Ameriflux
# FACE_RHIN

data.set <- "NARR"
fcn.data <- "NARR"

# Select username, host and directory folder for data

username <- ""
raw.host <- "geo.bu.edu"
dir      <- "/projectnb/dietzelab/pecan.data/input/"

# Set start and end dates (when possible otherwise NA)

start_year <- 1979 
end_year   <- 2013

# Download raw data? If not, specify raw.id
# NARR raw.id on geo = 285

raw    <- FALSE
raw.id <- 285

# Set site id number when possible (not possible for NARR)
# RHIN: 1000000003
# ORNL: 854

# site.id <- 1000000003


#######################################################
# Change to CF standards? If not, specify cf.id
# NARR cf.id on geo = 288

cf    <- FALSE
cf.id <- 288

# Permute data? If not, specify perm.id
# NARR perm.id on geo = 1000000023

perm    <- FALSE
perm.id <- 1000000023

# Select extraction site

extract <- TRUE
newsite <- 776

# Select model. Currently ED2 and SIPNET

model <- "SIPNET"

# Run met workflow

source("~/pecan/modules/data.atmosphere/inst/scripts/met.workflow.gen.R")