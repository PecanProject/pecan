download.NARR <- function(outfolder,start_year,end_year){
  
  if(!file.exists(outfolder)) dir.create(outfolder)
  
  start_year <- as.numeric(start_year)
  end_year   <- as.numeric(end_year)
  
  # Download Raw NARR from the internet
  
  vlist <- c("pres.sfc", "dswrf", "dlwrf", "air.2m", "shum.2m", "prate","vwnd.10m","uwnd.10m")
  
  for (v in vlist){
    for (year in seq(end_year,start_year,by=-1)){
      system(paste("wget -c -P ", outfolder ," ftp://ftp.cdc.noaa.gov/Datasets/NARR/monolevel/",v,".", year,".nc",sep=""))
    }    
  }
} 
  
  