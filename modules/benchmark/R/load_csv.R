##' load_csv
##'
##' @param data.path character, file path to the .csv file
##' @param format list, config list containing 
##' \itemize{
##'   \item \code{header},    numeric for number of header rows in file
##'   \item \code{skip},       numeric for skip argument of utils::read.csv 
##'   \item \code{na.strings}, character vector for na.strings argument
##'                           of utils::read.csv
##'   }
##' @param site list, currently ignored
##' @param vars column names to return. If NULL, returns all columns
##' 

##' @return a data frame with the processed data of the .csv file

##' 
##' @author Betsy Cowdery
##' @export
load_csv <- function(data.path, format, site, vars = NULL) {
  
  data.path <- sapply(data.path, function(x) dir(dirname(x), basename(x), full.names = TRUE))
  
  if (format$header == 0 | format$header == 1) {
    dat <- utils::read.csv(data.path, skip = format$skip, na.strings = format$na.strings, 
                    as.is = TRUE, check.names = FALSE, header = as.logical(format$header))
  } else if (format$header > 1) {
    dat <- utils::read.csv(data.path, skip = format$skip, na.strings = format$na.strings,
                    as.is = TRUE, check.names = FALSE, header = TRUE)
    dat <- dat[-c(1:format$header - 1), ]
  } else {
    dat <- utils::read.csv(data.path, skip = format$skip, na.strings = format$na.strings,
                    as.is = TRUE, check.names = FALSE)
  }
  
  if(!is.null(vars)){
    return(dplyr::select(dat, dplyr::one_of(vars)))
  }else{
    return(dat)
  }

} # load_csv
