##' Acceptance rate
##'
##' @name arate
##' @title arate 
##' @export
##'
##' @author Michael Dietze
arate <- function(x) {
  return(1 - (sum(diff(x) == 0, na.rm = TRUE) / (length(x) - 1)))
} # arate
