extract.success <- function(in.path,in.prefix,outfolder){
  
  in.path <- as.character(in.path)
  in.prefix <- as.character(in.prefix)
  outfolder <- as.character(outfolder)
  
  infiles = dir(in.path,in.prefix)
  infiles = files[grep(pattern="*.nc",infiles)]
  
  outfiles = dir(in.path,in.prefix)
  outfiles = files[grep(pattern="*.nc",outfiles)]
  
  
  if( length(outfiles) != length(infiles) || length(outfiles) == 0){
    logger.error("Conversion was not successful") 
    return(FALSE)
  }else{return(TRUE)}
}
