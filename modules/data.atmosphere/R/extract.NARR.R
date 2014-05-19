# extract.NARR <- function(slat,slon,infolder,infile,outfolder,start_year=1979,end_year=2012)
# args = c(pkg,fcn,dbfile$file_path,dbfile$file_name,outfolder)#,...) 

extract.NARR <- function(in.path,in.prefix,outfolder,slat,slon,start_year,end_year){
  
  in.path <- as.character(in.path)
  in.prefix <- as.character(in.prefix)
  outfolder <- as.character(outfolder)
  
  slat <- as.numeric(slat)
  slon <- as.numeric(slon)
  start_year <- as.numeric(start_year)
  end_year <- as.numeric(end_year)
  
  ## get file names
  files = dir(in.path,in.prefix)
  files = files[grep(pattern="*.nc",files)]
  
  if(length(files) == 0) {
    ## send warning
    
    return(NULL)
  }  
  
  if(!file.exists(outfolder)){
    dir.create(outfolder)
  }
  
  # Find closest coordinates to site
  close <- closest_xy(slat, slon,in.path,in.prefix)
  x <- close$x
  y <- close$y
  
  for (year in seq(end_year,start_year,by=-1)){
    
    next.file = paste0(in.path,"/",in.prefix,year, ".nc")
    if(file.exists(next.file)){
      system(paste0("ncks -d x,",x,",",x, " -d y,",y,",",y," ",next.file," ",outfolder,"/",next.file))
      # } else { print(paste(next.file,"DOES NOT EXIST"))
    }
    
  }
  
}