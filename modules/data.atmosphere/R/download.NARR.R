##' Download NARR files
##'
##' @name download.NARR
##' @title download.NARR
##' @export
##' @param outfolder
##' @param start_year
##' @param end_year
##' 
##' @author ??
download.NARR <- function(outfolder, start_date, end_date, overwrite=FALSE){
  
  require(lubridate) #is this necessary?
  
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  start_year <- year(start_date)
  end_year   <- year(end_date)
  
  # Download Raw NARR from the internet
  
  vlist <- c("pres.sfc", "dswrf", "dlwrf", "air.2m", "shum.2m", "prate","vwnd.10m","uwnd.10m")
  ylist <- seq(end_year,start_year,by=-1)
  
  #   cmd <- as.list(rep("", length(vlist)*length(ylist)+1))
  #   cmd[1] <- paste0("mkdir -p ", outfolder) 
  
  # cmd <- paste0("mkdir -p ", outfolder)
  
  system(paste0("mkdir -p ", outfolder))
  
  #   i <- 1
  for (v in vlist) {
    for (year in ylist) {
      filename <- paste0(v,".", year,".nc")
      new.file <- file.path(outfolder, filename)
      #       i <- i+1
      #       cmd[i] <- paste0("wget -c ftp://ftp.cdc.noaa.gov/Datasets/NARR/monolevel/",v,".", year,".nc"," -O ",new.file)
      # cmd <- paste(cmd, "; ", paste0("wget -c ftp://ftp.cdc.noaa.gov/Datasets/NARR/monolevel/",v,".", year,".nc"," -O ",new.file))
    system(paste0("wget -c ftp://ftp.cdc.noaa.gov/Datasets/NARR/monolevel/",v,".", year,".nc"," -O ",new.file))
    }
  }
  
#   Rfcn     <- "pecan/scripts/Rfcn.R"
#   host     <- system("hostname",intern=TRUE)
#   
#   if(raw.host %in% c("localhost",host)){
#     ## if the machine is local, run conversion function
#     system(cmd)
#   }else{
#     ## if the machine is remote, run conversion remotely
#     usr = ifelse(is.null(username) | username=="","",paste0(username,"@"))
#     system2("ssh",paste0(usr,paste(raw.host,cmd)))
#   }
}
  