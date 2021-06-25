#' Convert variables from ICOS Drought 2018 product to CF format.
#'
#' @param in.path path to the input Drought 2018 CSV file
#' @param in.prefix name of the input file
#' @param outfolder path to the directory where the output file is stored. If specified directory does not exists, it is created.
#' @param start_date start date of the input file
#' @param end_date end date of the input file
#' @param format format is data frame or list with elements as described below
#'   REQUIRED:
#'   format$header = number of lines of header
#'   format$vars is a data.frame with lists of information for each variable to read, at least airT is required
#'     format$vars$input_name = Name in CSV file
#'     format$vars$input_units = Units in CSV file
#'     format$vars$bety_name = Name in BETY
#'   OPTIONAL:
#'   format$lat = latitude of site
#'   format$lon = longitude of site
#'   format$na.strings = list of missing values to convert to NA, such as -9999
#'   format$skip = lines to skip excluding header
#'   format$vars$column_number = Column number in CSV file (optional, will use header name first)
#' Columns with NA for bety variable name are dropped. 
#' @param overwrite overwrite should existing files be overwritten. Default False.
#' @return information about the output file
#' @export
#'

met2CF.Drought2018 <- function(in.path, in.prefix, outfolder, start_date, end_date, format, overwrite = FALSE){
  results <- PEcAn.data.atmosphere::met2CF.csv(in.path, in.prefix, outfolder, start_date, end_date, format, overwrite = overwrite)
  return(results)
}