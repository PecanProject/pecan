##' @name load_rds
##' @title load_rds
##' @export
##' @param data.path character
##' @param format list, not used, for compatibility
##' @param site not used, for compatibility
##' @param vars optional variable names to load. if NULL, returns all variables in file
##' 
##' @author Istem Fer
load_rds <- function(data.path, format, site, vars = NULL) {
  
  data.path <- sapply(data.path, function(x) dir(dirname(x), basename(x), full.names = TRUE))
  
  dat <- readRDS(data.path)

  if(!is.null(vars)){
    return(dplyr::select(dat, dplyr::one_of(vars)))
  }else{
    return(dat)
  }
  
} # load_rds
