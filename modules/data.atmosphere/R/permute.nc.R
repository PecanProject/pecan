permute.nc <- function(in.path,prefix,out.path){
  
  ## get file names
  
  files = dir(in.path,prefix)
  files = files[grep(pattern="*.nc",files)]
  
  if(length(files) == 0) return(NULL)
  
  if(!file.exists(out.path)) dir.create(out.path)
  
  for(i in 1:length(files)){    
    
    infile = file.path(in.path,files[i])
    tempfile = file.path(out.path,paste0(files[i],"_temp"))
    outfile = file.path(out.path,files[i])
    
    
    if(file.exists(infile)==TRUE && file.exists(outfile)==FALSE){
      system(paste0("nccopy -k 3 -u -c time/8,y/277,x/349 ",infile," ",tempfile))
      system(paste0("ncpdq --no_tmp_fl -h -O -a y,x,time ",tempfile," ",outfile))
    }
  }
}