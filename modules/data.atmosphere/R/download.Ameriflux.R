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
download.Ameriflux <- function(start_year, end_year, site_id, in.prefix, outfolder){#, dbparams, con, raw.host){
  
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  start_year <- as.numeric(start_year)
  end_year   <- as.numeric(end_year)
  
  # Download Ameriflux L2 .nc from internet
  for(i in start_year:end_year){
    year <- i 
    url <- paste0("http://cdiac.ornl.gov/ftp/ameriflux/data/Level2/Sites_ByID/",
                  site_id,"with_gaps/",in.prefix,"_",year,"_L2_WG_V003.nc")
    download.file(url,paste0(outfolder,in.prefix,"_",year,"_L2_WG_V003.nc"))
  }
  
  outname = tail(unlist(strsplit(outfolder,'/')),n=1)
  
  check <- input.name.check(outname, con, dbparams)
  if(is.null(check)==FALSE){
    logger.error('Input is already in the database.')
    raw.id <- check
    return(raw.id)
  }
  
  start_date <- paste0(start_year,"-01-01 00:00:00")
  end_date   <- paste0(end_year,"-12-31 23:59:00")
  
  formatname <- 'Ameriflux'
  mimetype <- 'application/x-netcdf'
  filename <- paste0(outfolder,in.prefix)
  
  newinput <- dbfile.input.insert(filename, 
                                  siteid = site_id, 
                                  startdate = paste(input$start_date), 
                                  enddate = paste(input$end_date), 
                                  mimetype, 
                                  formatname,
                                  parentid = NA,
                                  con = con,
                                  hostname = raw.host) 
  return(newinput$input.id)
}
