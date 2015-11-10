load.csv <- function(data.path, format, start_year, end_year, site){
  
  
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
  
  return(dat)
  
}