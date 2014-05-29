extract.success <- function(in.path,in.prefix,outfolder){
  
  require("PEcAn.utils")
  
  in.path <- as.character(in.path)
  in.prefix <- as.character(in.prefix)
  outfolder <- as.character(outfolder)
  
  infiles = dir(in.path,in.prefix)
  infiles = infiles[grep(pattern="*.nc",infiles)]
  
  outfiles = dir(outfolder,in.prefix)
  outfiles = outfiles[grep(pattern="*.nc",outfiles)]
  
  
  if( length(outfiles) != length(infiles) || length(outfiles) == 0){
    s = FALSE
  }else{
    s = TRUE
  }
  system2("echo",paste(s))
}
