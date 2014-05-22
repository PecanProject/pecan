met2cf.NARR <- function(in.path,in.prefix,outfolder){

  cmdArgs = paste(c(in.path,in.prefix,outfolder),collapse=" ")
  
  fcn = system.file("inst/scripts/nc_formatting.sh", package = "PEcAn.data.atmosphere")
  system(paste(fcn,cmdArgs))

}