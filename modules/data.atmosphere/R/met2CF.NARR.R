met2CF.NARR <- function(in.path,in.prefix,outfolder){
  
  script <- paste0("inst/scripts/CF.",in.prefix,"sh")
  cmdArgs = paste(c(in.path,in.prefix,outfolder),collapse=" ")
  
  fcn = system.file(script, package = "PEcAn.data.atmosphere")
  system(paste(fcn,cmdArgs))
  
}