##' @title Download PalEON met ensemble files
##'
##' @export
##' @param outfolder
##' @param start_year
##' @param end_year
##' 
##' @author Betsy Cowdery, Mike Dietze
download.PalEON_ENS <- function(sitename, outfolder, start_date, end_date, overwrite = FALSE, ...) {
  
  ## parse dates
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date   <- as.POSIXlt(end_date, tz = "UTC")
  start_year <- lubridate::year(start_date)
  end_year   <- lubridate::year(end_date)
  ylist      <- start_year:end_year
  
  ## install iCommands: 
  ##  wget ftp://ftp.renci.org/pub/irods/releases/4.1.9/ubuntu14/irods-icommands-4.1.9-ubuntu14-x86_64.deb
  ##  sudo dpkg -i irods-icommands-4.1.9-ubuntu14-x86_64.deb
  ##  iinit
  ## then follow https://pods.iplantcollaborative.org/wiki/display/DS/Setting+Up+iCommands to connect
  
  ## Get the data from iPlant
  ##   iget /iplant/home/crollinson/paleon/phase3_met_drivers/test_ensemble -r -T -P
  ##
  ###### NEED TO AUTOMATE !!!!
  
  
  ## extract the data and loop over ensemble members
  dlpath <- "/home/carya/test_ensemble/"  ## download path
  setwd(dlpath)
  ens_zip <- dir(dlpath,pattern="tar.bz2",)
  results <- list()
  for(i in seq_along(ens_zip)){
    system2("tar",paste("-xvjf",ens_zip[i]))  ## unzip file
    ens_folder <- strsplit(basename(ens_zip[i]),"\\.")[[1]][1]
    
    ens_files <- dir(ens_folder)
    rows <- length(ens_files)
    ens_years <- sapply(strsplit(ens_files,"_",fixed=TRUE),function(n){
                  as.numeric(sub(".nc","",n[length(n)]))
                 })
    
    results[[i]] <- data.frame(file = ens_files, 
                          host = rep(PEcAn.remote::fqdn(),rows), 
                          mimetype = rep("application/x-netcdf",rows), 
                          formatname = rep("ALMA",rows),  ## would really like to switch to CF
                          startdate = paste0(ens_years, "-01-01 00:00:00"), 
                          enddate = paste0(ens_years, "-12-31 23:59:59"), 
                          dbfile.name = "PalEON_ENS", 
                          stringsAsFactors = FALSE)
  }
  
  if(length(results) == 1) results <- results[[1]] ## flatten to single met rather than ensemble
  
  return(invisible(results))
} # download.PalEON_ENS
