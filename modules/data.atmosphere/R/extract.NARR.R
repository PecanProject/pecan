# extract.NARR <- function(slat,slon,infolder,infile,outfolder,start_year=1979,end_year=2012)

# args = c(pkg,fcn,dbfile$file_path,dbfile$file_name,outfolder)#,...) 
extract.NARR <- function(in.path,in.prefix,outfolder){

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
  close <- closest_xy(slat, slon,infolder,infile)
  x <- close$x
  y <- close$y
  
  for (year in seq(end_year,start_year,by=-1)){

    next.file = paste0(infolder,"/",year, ".nc")
    if(file.exists(next.file)){
     system(paste0("ncks -d x,",x,",",x, " -d y,",y,",",y," ",next.file," ",outfolder,"/",year,"_sub.nc"))
     # } else { print(paste(next.file,"DOES NOT EXIST"))
    }

  }

}