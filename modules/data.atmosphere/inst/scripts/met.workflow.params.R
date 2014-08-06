# Database setup
driver   <- "PostgreSQL"
user     <- "bety"
dbname   <- "bety"
password <- "bety"
host     <-  "psql-pecan.bu.edu"

source("/home/ecowdery/pecan/modules/data.atmosphere/R/download.FACE.R")
source("/home/ecowdery/pecan/modules/data.atmosphere/R/met2CF.FACE.R")
source("/home/ecowdery/pecan/db/R/dbfiles.R")
source("utils/R/convert.input.R")

#######################################################
# Choose data set. 
# Current choices: 
# NARR
# Ameriflux
# FACE_RHIN

data.set <- "FACE_RHIN"
fcn.data <- unlist(strsplit(data.set,"_"))[[1]]

# Select host and directory folder for data

raw.host <- "geo.bu.edu"
dir      <- "/projectnb/dietzelab/pecan.data/input/"

# Set start and end dates (when possible)

start_year <- NA 
end_year   <- NA 

# Download raw data? If not, specify raw.id
# NARR raw.id on geo = 285

raw    <- TRUE

#raw.id <- 285

# Set site id number when possible
# RHIN: 1000000003
# ORNL: 854

site.id <- 1000000003

# Set username (otherwise NULL or "")
  
username <- ""

#######################################################
# Change to CF standards? If not, specify cf.id
# NARR cf.id on geo = 288

cf    <- TRUE
# cf.id <- 288

# Permute data? If not, specify perm.id
# NARR perm.id on geo = 1000000023

perm    <- FALSE
perm.id <- 1000000023

# Select extraction site

extract <- TRUE
newsite <- 622

# Select model. Currently ED2 and SIPNET

model <- "SIPNET"

# Move data from geo to pecan2
# rsync -avz ssh /projectnb/dietzelab/pecan.data/input/NARR_SIPNET_site_0-622 ecowdery@pecan2.bu.edu:/fs/data1/pecan.data/input/


# Run met workflow

# source(met.workflow.gen)