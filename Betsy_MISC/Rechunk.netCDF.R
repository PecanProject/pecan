Rechunk.netCDF 
  
  # Rechunk dimensions and convert the  time dimension from unlimited to fixed.
  
  in.path <- "/projectnb/cheas/pecan.data/input/NARR_CF/"
  prefix <- "NARR."
  out.path <- "/projectnb/cheas/pecan.data/input/NARR_CF_Rechunk/"
  
  ## get file names
  files = dir(in.path,prefix)
  files = files[grep(pattern="*.nc",files)]
  
  if(length(files) == 0) {
    return(NULL)
  }  
  
  if(!file.exists(out.path)){
    dir.create(out.path)
  }
  
  ## Rechunk dimensions and convert the  time dimension from unlimited to fixed.
  
  for(i in 1:length(files)){    
    infile = file.path(in.path,files[i])
    outfile = file.path(out.path,files[i])
    if(file.exists(infile)==TRUE && file.exists(outfile)==FALSE){
      system(paste0("nccopy -k 3 -u -c time/160708,lat/360,lon/720 ",infile," ",outfile))
    }
  }


