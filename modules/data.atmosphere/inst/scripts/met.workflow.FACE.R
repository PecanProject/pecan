# Load libraries

require(PEcAn.all)
require(PEcAn.data.atmosphere)
require(RPostgreSQL)

#--------------------------------------------------------------------------------------------------#
# Setup database connection

driver   <- "PostgreSQL"
user     <- "bety"
dbname   <- "bety"
password <- "bety"
host     <- "psql-pecan.bu.edu"
for (i in dbListConnections(PostgreSQL())) db.close(i);
dbparms <- list(driver=driver, user=user, dbname=dbname, password=password, host=host)

#--------------------------------------------------------------------------------------------------#
# Choose data set. 

data.set  <- "FACE_RHIN"
fcn.data  <- "FACE"
site.name <- "RHIN"

# Select username, host and directory folder for data

username <- ""
raw.host <- "geo.bu.edu"
dir      <- "/projectnb/dietzelab/pecan.data/input/"

# Set start and end dates (when possible otherwise NA)

start_year <- NA 
end_year   <- NA

site.id <- 1000000008

###########################
# Download raw data? If not, specify raw.id

raw    <- FALSE

if (raw == TRUE){
  con        <- db.open(dbparms)
  outfolder  <- paste0(dir,data.set,"/")
  pkg        <- "PEcAn.data.atmosphere"
  fcn        <- paste0("download.",fcn.data)
  
  args <- list(site.name,outfolder,pkg,raw.host,start_year,end_year,site.id,dbparms,con) #fix this later
  
  raw.id <- do.call(fcn,args)
}

raw.id <- c(1000000084,1000000083)

#--------------------------------------------------------------------------------------------------#
# Loop over treatments to do CF and then ED2

# HACK TIME!
remove(i)
i <- 2
l <- list()



case   <- c("a","e") # this shouldnt be here ... 

for(i in 1:length(raw.id)){
  
  con       <- db.open(dbparms)
  input.id  <-  raw.id[i]
  outfolder <-  paste0(dir,data.set,"_",case[i],"_CF/")
  pkg       <- "PEcAn.data.atmosphere"
  fcn       <-  paste0("met2CF.",fcn.data)
  write     <-  TRUE
  
  cf.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,con)
  
  con     <- db.open(dbparms)
  lst <- site.lst(newsite,con)
  
  # Convert to model format
  input.id  <- cf.id
  outfolder <- paste0(dir,data.set,"_",model,"_site_",str_ns,"/")
  pkg       <- paste0("PEcAn.",model)
  fcn       <- paste0("met2model.",model)
  write     <- TRUE
  overwrite <- ""
  
  model.id <- convert.input(input.id,outfolder,pkg,fcn,write,username,con,lst=lst,overwrite=overwrite)
}
  
  
  
  
}


