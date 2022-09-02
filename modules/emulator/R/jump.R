##' @name jump
##' @title jump
##' @export
##'
##' @param ic optional data vector
##' @param rate target acceptance rate
##' @param ... Addtional arguments
##' 
##' @author Michael Dietze
jump <- function(ic = 0, rate = 0.4, ...) {
  return(methods::new("jump", history = ic, arate = 0, target = rate))
} # jump

##' multivariate version
##' @title mvjump 
##' @export
##' 
##' @param ic optional data vector
##' @param rate target acceptance rate
##' @param nc NetCDF object containing target variable
##' @param ... Additional arguments
mvjump <- function(ic = 0, rate = 0.4, nc = 2, ...) {
  icm <- (matrix(ic, nrow = 1, ncol = nc))
  return(methods::new("mvjump", history = icm, arate = 0, target = rate))
} # mvjump
