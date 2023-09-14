##' @name load_tab_separated_values
##' @title load_tab_separated_values
##' @export
##' @param data.path character
##' @param format list
##' @param start_year numeric
##' @param end_year numeric
##' @param site list
##' 
##' @author Betsy Cowdery, Mike Dietze
load_tab_separated_values <- function(data.path, format, site=NULL, vars = NULL) {
  ## load's mime-type = text/tab-separated-values
  
  data.path <- sapply(data.path, function(x) dir(dirname(x), basename(x), full.names = TRUE))
  
  if (format$header == 0) {
    dat <- utils::read.table(data.path, sep="\t",skip = format$skip, na.strings = format$na.strings, 
                      as.is = TRUE, check.names = FALSE, header = FALSE)
    colnames(dat)[format$vars$column_number] <- format$vars$input_name
  } else if (format$header == 1) {
    dat <- utils::read.table(data.path, sep="\t",skip = format$skip, na.strings = format$na.strings, 
                    as.is = TRUE, check.names = FALSE, header = TRUE)
  } else if (format$header > 1) {
    dat <- utils::read.table(data.path, sep="\t",skip = format$skip, na.strings = format$na.strings,
                    as.is = TRUE, check.names = FALSE, header = TRUE)
    dat <- dat[-c(1:format$header - 1), ]
  } else {
    dat <- utils::read.table(data.path, sep="\t",skip = format$skip, na.strings = format$na.strings,
                    as.is = TRUE, check.names = FALSE)
  }
  
  if(!is.null(vars)){
    return(dplyr::select(dat, dplyr::one_of(vars)))
  }else{
    return(dat)
  }

} # load_tab_separated_values
