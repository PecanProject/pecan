##' Get meteorology variables from Ameriflux LBL and convert to netCDF CF format
##'
##' @name met2CF.AmerifluxLBL
##' @title met2CF.AmerifluxLBL
##' @export
##' @param in.path location on disk where inputs are stored
##' @param in.prefix prefix of input and output files
##' @param outfolder location on disk where outputs will be stored
##' @param start_date the start date of the data to be downloaded (will only use the year part of the date)
##' @param end_date the end date of the data to be downloaded (will only use the year part of the date)
##' @param overwrite should existing files be overwritten
##' @param verbose should ouput of function be extra verbose
##'
##' @author Ankur Desai
met2CF.AmerifluxLBL <- function(in.path, in.prefix, outfolder, start_date, end_date, format,
                             overwrite = FALSE, verbose = FALSE, ...) {

##Determine if file is in old or new format
  file_version <- substr(result$dbfile.name,nchar(result$dbfile.name)-1,nchar(result$dbfile.name))
  if (file_version=='-1') {
    ## File is in pre 2017 Ameriflux format, send to met2CF
    results <- PEcAn.data.atmosphere::met2CF.csv(in.path, in.prefix, outfolder,start_date, end_date,format, overwrite=overwrite)
  } else {
    ##read header (second line in file)
    ## for each input_name, grep for one that starts the same, swap the name of the first one (or sort?)
    ##format$vars$input_name
    ## then call met2CF
  }

## FUTURE: choose height
}