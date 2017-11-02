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
met2CF.AmerifluxLBL <- function(in.path, in.prefix, outfolder, start_date, end_date,
                             overwrite = FALSE, verbose = FALSE, ...) {

##Determine if file is in old or new format
##if in old format, send to met2CF.CSV
##create format record here by hand
##if in new format, work here, change format record
## FUTURE: choose height
  
  
}