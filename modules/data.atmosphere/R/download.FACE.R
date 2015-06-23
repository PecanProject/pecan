##' Download Raw FACE data from the internet
##'
##' @name download.FACE
##' @title download.FACE
##' @export
##' @param sitename
##' @param outfolder
##' @param start_year
##' @param end_year
##' 
##' @author Betsy Cowdery

download.FACE <- function(sitename,outfolder, start_date, end_date, overwrite=FALSE){
  # download.FACE <- function(data.set,outfolder,pkg,raw.host,start_year,end_year,site.id,dbparams,con){
  
  require(lubridate)
  require(PEcAn.utils)
  require(data.table)
  
  start_date <- as.POSIXlt(start_date, tz = "GMT")
  end_date <- as.POSIXlt(end_date, tz = "GMT")
  
  site <- site_from_tag(sitename, "FACE")
  
  # make sure output folder exists
  if(!file.exists(outfolder)){
    dir.create(outfolder, showWarnings=FALSE, recursive=TRUE)
  }
  
  raw.file <- paste0(site,"_forcing_h.nc")
  out.file <- file.path(outfolder, paste0("FACE_",raw.file))
  
  # url where Ameriflux data is stored
  url <- paste0("ftp://cdiac.ornl.gov/.private/eCO2_Modelling/Site_Data/",site,"/", raw.file)
  print(url)
  system(paste("wget -c ", url, " -O ", out.file))
  
  results <- data.frame(file=out.file, 
                        host=fqdn(),
                        mimetype='application/x-netcdf', 
                        formatname='FACE',
                        startdate=start_date, 
                        enddate=end_date,
                        dbfile.name = 'FACE',
                        stringsAsFactors = FALSE)
  
  invisible(results)
  ######################
  
  #   if(is.na(start_year) |is.na(end_year)){
  #     j <- grep("YEAR =",years)
  #     start_year <- as.numeric(substr(unlist(strsplit(years[j],","))[1],nchar(unlist(strsplit(years[j],","))[1])-4,nchar(unlist(strsplit(years[j],","))[1]) ))
  #     end_year <- as.numeric(unlist(strsplit(years[length(years)-1],";"))[1])   
  #     start_date <- paste0(start_year,"-01-01 00:00:00")
  #     end_date   <- paste0(end_year,"-12-31 23:59:00")
  #   }
  
  
  
} 
