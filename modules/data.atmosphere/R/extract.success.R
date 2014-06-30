extract.success <- function(in.path,in.prefix,outfolder){
  
  require("PEcAn.utils")
  
  in.path <- as.character(in.path)
  in.prefix <- as.character(in.prefix)
  outfolder <- as.character(outfolder)

  infiles = dir(in.path,in.prefix)
  infiles.nc = infiles[grep(pattern="*.nc",infiles)]
  
  outfiles = dir(outfolder)
  outfiles.nc = outfiles[grep(pattern="*.nc",outfiles)]
  outfiles.h5 = outfiles[grep(pattern="*.h5",outfiles)]
  
  
  if(length(outfiles) == 0){
    s = FALSE
  }else if(length(outfiles.nc) == length(infiles) || length(outfiles.h5) == length(infiles)*12){
    s = TRUE
  }
  return(s)
}
