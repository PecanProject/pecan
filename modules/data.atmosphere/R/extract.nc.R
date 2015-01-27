##' Given latitude and longitude coordinates, extract site data from NARR file
##'
##'
##' @name extract.nc 
##' @title extract.nc 
##' @export
##' @author Betsy Cowdery

extract.nc <- function(in.path,in.prefix,outfolder,start_date,end_date,slat,slon,newsite){
  
  require("PEcAn.utils")
  
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
  
  for(i in 1:length(files)){    
    infile = file.path(in.path,files[i])
    outfile = file.path(outfolder,files[i])
    if(file.exists(infile)==TRUE && file.exists(outfile)==FALSE){
      system(paste0("ncks -d x,",x,",",x, " -d y,",y,",",y," ",infile," ",outfile))
    }
  }
}