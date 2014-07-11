
# 
# Generalized workflow based on met.workflow.NARR.R
#
#--------------------------------------------------------------------------------------------------#
# Load libraries

require(PEcAn.all)
require(PEcAn.data.atmosphere)
require(RPostgreSQL)

#--------------------------------------------------------------------------------------------------#
# Setup database connection
for (i in dbListConnections(PostgreSQL())) db.close(i)
dbparms <- list(driver="PostgreSQL" , user = "bety", dbname = "bety", password = "bety", host = "psql-pecan.bu.edu")
con     <- db.open(dbparms)

#--------------------------------------------------------------------------------------------------#
# Download raw data from the internet 

#data.set <- "NARR"
data.set <- "FACE"

outfolder  <- paste0("/projectnb/cheas/pecan.data/input/",data.set,"/")
pkg        <- "PEcAn.data.atmosphere"
raw.host   <- "geo.bu.edu"
fcn        <- paste0("download.",data.set)

# Dates for NARR
# start_year <- 1998 
# end_year   <- 2008 

args <- list(data.set,outfolder,pkg,raw.host) # start_year,end_year)

raw.id <- do.call(fcn,args)
# NARR raw.id should be 285

#--------------------------------------------------------------------------------------------------#
# Change to CF Standards

input.id  <-  raw.id
outfolder <-  paste0("/projectnb/cheas/pecan.data/input/",data.set,"_CF/")
pkg       <- "PEcAn.data.atmosphere"
fcn       <-  paste0("met2CF.",data.set)
write     <-  TRUE
username  <- ""

cf.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,dbparms,con) # doesn't update existing record
# NARR cf.id should be 288

#--------------------------------------------------------------------------------------------------#
# Rechunk and Permute
input.id  <-  cf.id
outfolder <-  paste0("/projectnb/cheas/pecan.data/input/",data.set,"_CF_Permute/")
pkg       <- "PEcAn.data.atmosphere"
fct       <- "permute.nc"
write     <-  TRUE
username  <- ""

perm.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,dbparms,con)
#NARR_perm.id should be 1000000023

#--------------------------------------------------------------------------------------------------#
# Extract for location
input.id <- perm.id
newsite  <- 700
str_ns   <- paste0(newsite %/% 1000000000, "-", newsite %% 1000000000)

outfolder <- paste0("/projectnb/cheas/pecan.data/input/",data.set,"_CF_site_",str_ns,"/")
pkg       <- "PEcAn.data.atmosphere"
fcn       <- "extract.nc"
write     <- TRUE
username  <- ""

extract.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,dbparms,con,newsite = newsite)

#--------------------------------------------------------------------------------------------------#
# Prepare for ED Model

# Acquire lst (probably a better method, but this works for now)
lst <- site.lst(newsite)

# Convert to ED format
input.id <- extract.id

outfolder <- paste0("/projectnb/cheas/pecan.data/input/",data.set,"_ED_site_",str_ns,"/")
pkg       <- "PEcAn.ED2"
fcn       <- "met2model.ED2"
write     <- TRUE
overwrite <- ""
username  <- ""

ED.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,dbparms,con,lst=lst,overwrite=overwrite)

#--------------------------------------------------------------------------------------------------#
# Clear old database connections
for (i in dbListConnections(PostgreSQL())) db.close(i)
