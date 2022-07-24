##' Acceptance rate
##'
##' @name arate
##' @title arate 
##' @export
##'
##' @param x Name of variable to plot on X axis
##' @author Michael Dietze
arate <- function(x) {
  return(1 - (sum(diff(x) == 0, na.rm = TRUE) / (length(x) - 1)))
} # arate
