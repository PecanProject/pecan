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
##' @param format is data frame or list with elements as described below
##' The AmerifluxLBL format is Bety record 5000000002
##' which could be returned from PEcAn.DB::query.format.vars(format.id=5000000002)
##' format is output from db/R/query.format.vars, and should have:
##'   REQUIRED:
##'   format$lat = latitude of site
##'   format$lon = longitude of site
##'   format$header = number of lines of header
##'   format$vars is a data.frame with lists of information for each variable to read, at least airT is required
##'     format$vars$input_name = Name in CSV file
##'     format$vars$input_units = Units in CSV file
##'     format$vars$bety_name = Name in BETY
##'   OPTIONAL:
##'   format$na.strings = list of missing values to convert to NA, such as -9999
##'   format$skip = lines to skip excluding header
##'   format$vars$column_number = Column number in CSV file (optional, will use header name first)
##' Columns with NA for bety variable name are dropped. 
##' Units for datetime field are the lubridate function that will be used to parse the date (e.g. \code{ymd_hms} or \code{mdy_hm}). 

##' @param overwrite should existing files be overwritten
##' @param verbose should ouput of function be extra verbose
##' @param ... further arguments, currently ignored
##'
##' @author Ankur Desai
met2CF.AmerifluxLBL <- function(in.path, in.prefix, outfolder, start_date, end_date, format,
                             overwrite = FALSE, verbose = FALSE, ...) {

##Determine if file is in old or new format based on filename ending in "-1" or not
## If in new format, then convert header input names based on file header
## Otherwise just call met2CF.csv as usual

  file_version <- substr(in.prefix,nchar(in.prefix)-1,nchar(in.prefix))
  if (file_version!='-1') {
##Open the file and read the first few lines to get header
    PEcAn.logger::logger.info("New Ameriflux format, updating format record")
    files <- dir(in.path, in.prefix, full.names = TRUE)
    files <- files[grep("*.csv", files)]
    if (length(files) == 0) {
      PEcAn.logger::logger.warn("No met files named ", in.prefix, "found in ", in.path)
      return(NULL)
    }
    if (length(files) > 1) {
      PEcAn.logger::logger.warn(length(files), ' met files found. Using first file: ', files[1])
      files <- files[1]
    }
    somedat <- utils::read.csv(files, 
                       header = TRUE,
                       skip = format$skip, 
                       na.strings = format$na.strings,
                       as.is = TRUE, 
                       check.names = FALSE,nrows=1)
    colname <- names(somedat)
    
    ##Take the original format and strip everything after _
    formatname <- format$vars$input_name
    removeunder <- regexpr("\\_[^\\_]*$",formatname)
    removethese <- which(removeunder!=-1)
    if (length(removethese)>0){
      formatname[removethese] = substr(formatname[removethese],replicate(length(removethese),1),removeunder[removethese]-1)
    }

    ##Loop over format names, match to new header and substitute in    
    for (i in 1:length(formatname)) {
      if (formatname[i]=="TIMESTAMP") {
        formatname[i]="TIMESTAMP_START"
      }
      if (nchar(formatname[i])==1) {
        ## to avoid overlap with single character and multi character variable names
        namesearch <- colname[which(colname==formatname[i])]
        if (length(namesearch)==0) {
          namesearch <- sort(colname[grep(paste0("^",formatname[i],"_"),colname)])
        }
      } else {
        namesearch <- sort(colname[grep(paste0("^",formatname[i]),colname)])
      }
      if (length(namesearch)>1) { 
        namesearch <- namesearch[1]
      }
      if (length(namesearch)==1) {
        loc <- which(colname %in% namesearch)  
        format$vars$column_number[i] <- loc
        format$vars$input_name[i] <- namesearch
      }
    }
    PEcAn.logger::logger.info(format$vars$input_name)
  }
  ##Call met2CF with either original or modified format record
  results <- PEcAn.data.atmosphere::met2CF.csv(in.path, in.prefix, outfolder,start_date, end_date,format, overwrite=overwrite)
## FUTURE: choose height based on tower height information
}