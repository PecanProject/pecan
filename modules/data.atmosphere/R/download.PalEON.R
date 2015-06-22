##' Download PalEON files
##'
##' @name download.PalEON
##' @title download.PalEON
##' @export
##' @param outfolder
##' @param start_year
##' @param end_year
##' 
##' @author Betsy Cowdery
download.PalEON <- function(outfolder, start_date, end_date, overwrite=FALSE){
  
  sites <- c("PBL","PDL","PHA","PHO","PMB","PUN")
  
  for(s in sites){
    if(grepl(s, outfolder)){
      outfolder <- file.path("/projectnb/dietzelab/paleon/met_regional/phase1a_met_drivers_v4.2",s)  
    } 
  }
  
  require(PEcAn.utils)
  require(lubridate)
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  start_year <- year(start_date)
  end_year   <- year(end_date)
  ylist <- start_year:end_year
  mlist <- 1:12
  
  vlist <- c("lwdown","precipf","psurf","qair","swdown","tair","wind")
  
  system(paste0("mkdir -p ", outfolder))
  
  V = length(vlist)
  Y = length(ylist)
  M = length(mlist)
  rows <- V*Y*M
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = "PalEON",
                        stringsAsFactors = FALSE)
  
  files = dir(outfolder)
  if (sum(!(vlist %in% files))>0){
    logger.error("Don't have all variables downloaded")
  }else{
    for(v in vlist){
      print(sprintf("Checking %s", v))
      for (y in ylist){
        for (m in mlist){
          file <- file.path(outfolder,v,sprintf("%s_%s_%04d_%02d.nc",site,v,y,m))
          if(!(file.exists(file))){
            logger.error("Missing met file")
          }
          row <- (which(vlist==v)-1)*Y*M + (which(ylist==y)-1)*M + m
          #print(row)
          results$file[row] <- outfolder
          results$host[row] <- fqdn()
          results$startdate[row] <- paste0(y,"-01-01 00:00:00")
          results$enddate[row] <- paste0(y,"-12-31 23:59:59")
          results$mimetype[row] <- 'application/x-netcdf'
          results$formatname[row] <- 'ALMA'
        }
      }
      print(sprintf("Finished %s", v))
    }
  }
  
  invisible(results)
  
}
