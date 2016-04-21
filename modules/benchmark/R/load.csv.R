##' @name load.csv
##' @title load.csv
##' @export
##' @param data.path
##' @param format
##' @param start_year
##' @param end_year
##' @param site
##' 
##' @author Betsy Cowdery

load.csv <- function(data.path, format, start_year, end_year, site, vars=NULL){
  
  
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