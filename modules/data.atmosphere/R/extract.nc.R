##' extract point from nc file
##'
##' given a lat/lon, create a new file for a point
##'
##' Uses netcdf operators
##' @title extract.nc 
##' @param indir  character, location of file
##' @param in.prefix character, file prefix
##' @param outdir character, output directory
##' @param lat numeric, latitude to extract
##' @param lon numeric, longitude to extract
##' @return nothing, creates new file as artifact
##' @author Betsy Cowdery, David LeBauer
extract.nc <- function(indir, in.prefix, outdir, lat, lon){
  
  library("PEcAn.utils")

  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  ## get file names
  files = dir(path = indir, pattern = "in.prefix*.nc",
      full.names = TRUE)
  
  if(length(files) == 0) {
    logger.error("No files in input location")
    return(NULL)
  }  
  
  ## if lat, lon integer, make decimal; required by nco
  if(lat %% 1 == 0) lat <- format(lat, nsmall = 1)
  if(lon %% 1 == 0) lon <- format(lon, nsmall = 1)
  
  for(file in files){
      outfile <- gsub(indir, oufolder, file)
      if(!file.exists(outfile)){
          nco.cmd <- paste0("ncks -d lat,",lat, " -d lon,",lon, infile, " ", outfile)
          system(nco.cmd)
      }
  }
}
