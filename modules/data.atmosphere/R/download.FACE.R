##' Download Raw FACE data from the internet
##'
##'
##' @param sitename sitename
##' @param outfolder location where output is stored
##' @param method Optional. Passed to download_file() function.  Use this to set custom programs such as ncftp to use when
##' downloading files from FTP sites
##' @param start_date desired start date YYYY-MM-DD
##' @param end_date desired end date YYYY-MM-DD
##' @param overwrite overwrite existing files? Default is FALSE
##' @param ... other inputs
##'
##' @author Betsy Cowdery
##' @export
download.FACE <- function(sitename, outfolder, start_date, end_date, overwrite = FALSE, method, ...) {
  # download.FACE <-
  # function(data.set,outfolder,pkg,raw.host,start_year,end_year,site.id,dbparams,con){
  
  start_date <- as.POSIXlt(start_date, tz = "UTC")
  end_date   <- as.POSIXlt(end_date, tz = "UTC")
  
  site <- site_from_tag(sitename, "FACE")
  
  # make sure output folder exists
  if (!file.exists(outfolder)) {
    dir.create(outfolder, showWarnings = FALSE, recursive = TRUE)
  }
  
  raw.file <- paste0(site, "_forcing_h.nc")
  out.file <- file.path(outfolder, paste0("FACE_", raw.file))
  
  # url where Ameriflux data is stored
  url <- paste0("ftp://cdiac.ornl.gov/.private/eCO2_Modelling/Site_Data/", site, "/", raw.file)
  print(url)
  PEcAn.utils::download_file(url,out.file,method)

  # return file info
  return(invisible(data.frame(file = out.file, 
                              host = PEcAn.remote::fqdn(), 
                              mimetype = "application/x-netcdf", 
                              formatname = "FACE", 
                              startdate = start_date, 
                              enddate = end_date,
                              dbfile.name = "FACE", 
                              stringsAsFactors = FALSE)))
  
  ######################
  
  #   if(is.na(start_year) |is.na(end_year)){
  #     j <- grep("YEAR =",years)
  #     start_year <- as.numeric(substr(unlist(strsplit(years[j],","))[1],nchar(unlist(strsplit(years[j],","))[1])-4,nchar(unlist(strsplit(years[j],","))[1]) ))
  #     end_year <- as.numeric(unlist(strsplit(years[length(years)-1],";"))[1])   
  #     start_date <- paste0(start_year,"-01-01 00:00:00")
  #     end_date   <- paste0(end_year,"-12-31 23:59:00")
  #   }
} # download.FACE
