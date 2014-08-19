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
dbparms <- list(driver=driver, user=user, dbname=dbname, password=password, host=host)
con     <- db.open(dbparms)

#--------------------------------------------------------------------------------------------------#
# Download raw data from the internet 

if (raw == TRUE){
outfolder  <- paste0(dir,data.set,"/")
pkg        <- "PEcAn.data.atmosphere"
fcn        <- paste0("download.",fcn.data)

args <- list(data.set,outfolder,pkg,raw.host,start_year,end_year,site.id,dbparms,con)

raw.id <- do.call(fcn,args)
}

#--------------------------------------------------------------------------------------------------#
# Change to CF Standards

if (cf == TRUE){
input.id  <-  raw.id
outfolder <-  paste0(dir,data.set,"_CF/")
pkg       <- "PEcAn.data.atmosphere"
fcn       <-  paste0("met2CF.",fcn.data)
write     <-  TRUE

cf.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,dbparms,con) # doesn't update existing record
}

#--------------------------------------------------------------------------------------------------#
# Rechunk and Permute

if (perm == TRUE){
input.id  <-  cf.id
outfolder <-  paste0(dir,data.set,"_CF_Permute/")
pkg       <- "PEcAn.data.atmosphere"
fct       <- "permute.nc"
write     <-  TRUE

perm.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,dbparms,con)
}


#--------------------------------------------------------------------------------------------------#
# Extract for location

if (extract == TRUE){
input.id <- perm.id
str_ns   <- paste0(newsite %/% 1000000000, "-", newsite %% 1000000000)
outfolder <- paste0("/projectnb/dietzelab/pecan.data/input/",data.set,"_CF_site_",str_ns,"/")
pkg       <- "PEcAn.data.atmosphere"
fcn       <- "extract.nc"
write     <- TRUE

extract.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,dbparms,con,newsite = newsite)
}

#--------------------------------------------------------------------------------------------------#
# Prepare for Model

if(nchar(model) >2){
  
# Acquire lst (probably a better method, but this works for now)
lst <- site.lst(newsite)

# Convert to ED format
input.id  <- extract.id
char
outfolder <- paste0(dir,data.set,"_ED_site_",str_ns,"/")
pkg       <- paste0("PEcAn.",model)
fcn       <- paste0("met2model.",model)
write     <- TRUE
overwrite <- ""

model.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,dbparms,con,lst=lst,overwrite=overwrite)
}

#--------------------------------------------------------------------------------------------------#
# Clear old database connections
for (i in dbListConnections(PostgreSQL())) db.close(i)


