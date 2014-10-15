##' Download Ameriflux L2 netCDF files
##'
##' @name download.Ameriflux
##' @title download.Ameriflux
##' @export
##' @param start_year
##' @param end_year
##' @param site_id
##' @param in.prefix
##' @param outfolder
##' 
##' @author Josh Mantooth
download.Ameriflux <- function(start_year, end_year, site_id, in.prefix, outfolder){
  
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  start_year <- as.numeric(start_year)
  end_year   <- as.numeric(end_year)
  
  # Download Ameriflux L2 .nc from internet
  for(i in start_year:end_year){
    year <- i 
    url <- paste0("http://cdiac.ornl.gov/ftp/ameriflux/data/Level2/Sites_ByID/",
                  site_id,"with_gaps/",in.prefix,"_",year,"_L2_WG_V005.nc")
    download.file(url,paste0(outfolder,in.prefix,"_",year,"_L2_WG_V005.nc"))
  }
}