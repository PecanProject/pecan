# 
# Generalized met workflow 
# Functional for downloading NARR, converting to CF, Rechunk/Permuting, extracting and prep for SIPNET
#--------------------------------------------------------------------------------------------------#
# Load libraries

require(PEcAn.all)
require(PEcAn.data.atmosphere)
require(RPostgreSQL)

#--------------------------------------------------------------------------------------------------#
# Setup database connection

for (i in dbListConnections(PostgreSQL())) db.close(i)
dbparms <- list(driver=driver, user=user, dbname=dbname, password=password, host=host)

#--------------------------------------------------------------------------------------------------#
# Download raw data from the internet 

if (raw == TRUE){
  con        <- db.open(dbparms)
  outfolder  <- paste0(dir,met,"/")
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- paste0("download.",met)
  
  args <- list(site.id, outfolder, start_date, end_date, overwrite=FALSE, verbose=FALSE, pkg,raw.host,dbparms,con)
  
  raw.id <- do.call(fcn,args)
}

#--------------------------------------------------------------------------------------------------#
# Change to CF Standards

if (cf == TRUE){
  con       <- db.open(dbparms)
  input.id  <-  raw.id
  outfolder <-  paste0(dir,data.set,"_CF/")
  pkg       <- "PEcAn.data.atmosphere"
  fcn       <-  paste0("met2CF.",fcn.data)
  write     <-  TRUE
  
  cf.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,con) # doesn't update existing record
}

#--------------------------------------------------------------------------------------------------#
# Rechunk and Permute

if (perm == TRUE){
  con       <- db.open(dbparms)
  input.id  <-  cf.id
  outfolder <-  paste0(dir,data.set,"_CF_Permute/")
  pkg       <- "PEcAn.data.atmosphere"
  fcn       <- "permute.nc"
  write     <-  TRUE
  
  perm.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,con)
}


#--------------------------------------------------------------------------------------------------#
# Extract for location

if (extract == TRUE){
  con       <- db.open(dbparms)
  input.id  <- perm.id
  str_ns    <- paste0(newsite %/% 1000000000, "-", newsite %% 1000000000)
  outfolder <- paste0("/projectnb/dietzelab/pecan.data/input/",data.set,"_CF_site_",str_ns,"/")
  pkg       <- "PEcAn.data.atmosphere"
  fcn       <- "extract.nc"
  write     <- TRUE
  
  extract.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,con,newsite = newsite)
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
  
  model.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,con,lst=lst,overwrite=overwrite)
}

#--------------------------------------------------------------------------------------------------#
# Clear old database connections
for (i in dbListConnections(PostgreSQL())) db.close(i)


