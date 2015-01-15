#---------------- Load libraries. -----------------------------------------------------------------#
require(PEcAn.all)
require(PEcAn.data.atmosphere)
require(RPostgreSQL)

#--------------------------------------------------------------------------------------------------#
# Clear old database connections
for (i in dbListConnections(PostgreSQL())) db.close(i)

#--------------------------------------------------------------------------------------------------#
# Download raw NARR from the internet 

rm(list = setdiff(ls(), lsf.str()))
outfolder  <- "/projectnb/cheas/pecan.data/input/NARR/"
start_year <- 1979
end_year   <- 2013
pkg        <- "PEcAn.data.atmosphere"
NARR.host  <- "geo.bu.edu"

NARR_raw.id <- raw.NARR(outfolder,start_year,end_year,pkg,NARR.host) 
# NARR_raw.id should be 285

#--------------------------------------------------------------------------------------------------#
# Change to CF Standards

if (cf == TRUE){
  con       <- db.open(dbparms)
  input.id  <-  raw.id
  outfolder <-  paste0(dir,met,"_CF/")
  pkg       <- "PEcAn.data.atmosphere"
  fcn       <-  paste0("met2CF.",met)
  write     <-  TRUE
  formatname <- 'CF Meteorology'
  mimetype <- 'application/x-netcdf'
  
  cf.id <- convert.input(input.id,outfolder,formatname,mimetype,site.id,start_date,end_date,pkg,fcn,write,username,con)
}

#--------------------------------------------------------------------------------------------------#
# Rechunk and Permute
input.id  <-  NARR_cf.id
outfolder <- "/projectnb/cheas/pecan.data/input/NARR_CF_Permute/"
write     <-  TRUE

if (perm == TRUE){
  con       <- db.open(dbparms)
  input.id  <-  cf.id
  outfolder <-  paste0(dir,data.set,"_CF_Permute/")
  pkg       <- "PEcAn.data.atmosphere"
  fcn       <- "permute.nc"
  write     <-  TRUE
  formatname <- 'CF Meteorology'
  mimetype <- 'application/x-netcdf'

  
  perm.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,con)
}

#--------------------------------------------------------------------------------------------------#
# Extract for location
input.id <- NARR_perm.id
newsite  <- 336
str_ns   <- paste0(newsite %/% 1000000000, "-", newsite %% 1000000000)

if (extract == TRUE){
  con       <- db.open(dbparms)
  input.id  <- perm.id
  str_ns    <- paste0(newsite %/% 1000000000, "-", newsite %% 1000000000)
  outfolder <- paste0("/projectnb/dietzelab/pecan.data/input/",data.set,"_CF_site_",str_ns,"/")
  pkg       <- "PEcAn.data.atmosphere"
  fcn       <- "extract.nc"
  write     <- TRUE
  formatname <- 'CF Meteorology'
  mimetype <- 'application/x-netcdf'
  
  extract.id <- convert.input(input.id,outfolder,formatname,mimetype,pkg,fcn,write,username,con,newsite = newsite)
}

#--------------------------------------------------------------------------------------------------#
# Prepare for Model

if(nchar(model) >2){
  
  con     <- db.open(dbparms)
  
  # Acquire lst (probably a better method, but this works for now)
  lst <- site.lst(newsite,con)
  
  # Convert to model format
  input.id  <- extract.id
  outfolder <- paste0(dir,data.set,"_",model,"_site_",str_ns,"/")
  pkg       <- paste0("PEcAn.",model)
  fcn       <- paste0("met2model.",model)
  write     <- TRUE
  overwrite <- ""
  
  model.id <- convert.input(input.id,outfolder,mod.formatname,mod.mimetype,pkg,fcn,write,username,con,lst=lst,overwrite=overwrite)
}

#--------------------------------------------------------------------------------------------------#
# Clear old database connections
for (i in dbListConnections(PostgreSQL())) db.close(i)
