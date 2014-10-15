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
download.NARR <- function(outfolder,start_year,end_year){
  
  if(!file.exists(outfolder)) dir.create(outfolder)
  
  start_year <- as.numeric(start_year)
  end_year   <- as.numeric(end_year)
  
  # Download Raw NARR from the internet
  
  vlist <- c("pres.sfc", "dswrf", "dlwrf", "air.2m", "shum.2m", "prate","vwnd.10m","uwnd.10m")
  
  for (v in vlist) {
    for (year in seq(end_year,start_year,by=-1)) {
      filename <- paste0(v,".", year,".nc")
      outfile <- file.path(outfolder, filename)
      if (!file.exists(outfile)) {
      url <- paste0("ftp://ftp.cdc.noaa.gov/Datasets/NARR/monolevel/",v,".", year,".nc")
        download.file(url, outfile)
      }
    }
  }
} 
  
  