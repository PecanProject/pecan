##' @name load_csv
##' @title load_csv
##' @export
##' @param data.path character
##' @param format list
##' @param start_year numeric
##' @param end_year numeric
##' @param site list
##' 
##' @author Betsy Cowdery
load_csv <- function(data.path, format, site, vars = NULL) {
  
  data.path <- sapply(data.path, function(x) dir(dirname(x), basename(x), full.names = TRUE))
  
  if (format$header == 0 | format$header == 1) {
    dat <- read.csv(data.path, skip = format$skip, na.strings = format$na.strings, 
                    as.is = TRUE, check.names = FALSE, header = as.logical(format$header))
  } else if (format$header > 1) {
    dat <- read.csv(data.path, skip = format$skip, na.strings = format$na.strings,
                    as.is = TRUE, check.names = FALSE, header = TRUE)
    dat <- dat[-c(1:header - 1), ]
  } else {
    dat <- read.csv(data.path, skip = format$skip, na.strings = format$na.strings,
                    as.is = TRUE, check.names = FALSE)
  }
  
  if(!is.null(vars)){
    return(dplyr::select(dat, one_of(vars)))
  }else{
    return(dat)
  }

} # load_csv
