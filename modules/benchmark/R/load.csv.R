##' @name load.csv
##' @title load.csv
##' @export
##' @param data.path character
##' @param format list
##' @param start_year numeric
##' @param end_year numeric
##' @param site list
##' 
##' @author Betsy Cowdery

load.csv <- function(data.path, format, site, vars=NULL){
  
  
  if (format$header == 0 | format$header == 1){
    dat <- read.csv(data.path, skip = format$skip, na.strings = format$na.strings, as.is=TRUE,
                    check.names = FALSE, header = as.logical(format$header))
  }else if (format$header > 1){
    dat <- read.csv(data.path, skip = format$skip, na.strings = format$na.strings, as.is=TRUE, 
                    check.names = FALSE, header = TRUE)
    dat <- dat[-c(1:header-1),]
  }else{
    dat <- read.csv(data.path, skip = format$skip, na.strings = format$na.strings, as.is=TRUE, 
                    check.names = FALSE)
  }
  
  if(!is.null(vars)){
    return(dat[vars])
  }else{
    return(dat)
  }
  
}