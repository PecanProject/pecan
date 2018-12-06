##' @name mean_over_larger_timestep 
##' @title Calculate benchmarking statistics
##' @param date.fine numeric
##' @param data.fine data.frame
##' @param date.coarse numeric
##' @export mean_over_larger_timestep
##' 
##' @author Betsy Cowdery, Michael Dietze
mean_over_larger_timestep <- function(date.coarse, date.fine, data.fine) {
  return(tapply(X = data.fine, INDEX = findInterval(date.fine, date.coarse), FUN = function(x) mean(x, na.rm = TRUE)))
} # mean_over_larger_timestep
