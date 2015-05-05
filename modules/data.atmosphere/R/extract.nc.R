##' Given latitude and longitude coordinates, extract site data from NARR file
##'
##'
##' @name extract.nc 
##' @title extract.nc 
##' @export
##' @author Betsy Cowdery

extract.nc <- function(in.path,in.prefix,outfolder,start_date,end_date,slat,slon,newsite){
  
  require("PEcAn.utils")
  require("lubridate")
  
  in.path <- as.character(in.path)
  in.prefix <- as.character(in.prefix)
  outfolder <- as.character(outfolder)
  slat <- eval(parse(text = slat))
  slon <- eval(parse(text = slon))
  
  ## get file names
  files = dir(in.path,in.prefix)
  files = files[grep(pattern="*.nc",files)]
  
  if(length(files) == 0) {
    logger.error("No files in input location")
    return(NULL)
  }  
  
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  # Find closest coordinates to site
  close <- closest_xy(slat, slon,in.path,in.prefix)
  x <- close$x
  y <- close$y
  print(c(x,y))
  
  start_year <- year(start_date)
  end_year <- year(end_date)
  rows <- end_year-start_year+1
  results <- data.frame(file=character(rows), host=character(rows),
                        mimetype=character(rows), formatname=character(rows),
                        startdate=character(rows), enddate=character(rows),
                        dbfile.name = in.prefix,
                        stringsAsFactors = FALSE)
  
  for(i in 1:rows){    
    file <- paste("NARR",start_year+i-1,"nc", sep=".")
    infile = file.path(in.path,file)
    outfile = file.path(outfolder,file)
    if(file.exists(infile)==TRUE && file.exists(outfile)==FALSE){
      system(paste0("ncks -d x,",x,",",x, " -d y,",y,",",y," ",infile," ",outfile))      
    }
    results$file[i] <- outfile
    results$host[i] <- fqdn()
    results$startdate[i] <- start_date
    results$enddate[i] <- end_date
    results$mimetype[i] <- 'application/x-netcdf'
    results$formatname[i] <- 'CF'
  }
  return(invisible(results))
}