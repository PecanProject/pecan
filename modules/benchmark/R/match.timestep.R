##' @name match.timestep 
##' @title Match time step
##' @param date.fine numeric
##' @param data.fine matrix
##' @param date.coarse numeric
##' @export match.timestep
##' 
##' @author Istem Fer
match.timestep <- function(date.coarse, date.fine, data.fine) {
  
  midpoints <- c(-Inf, head(as.numeric(date.fine), -1)) + c(0, diff(as.numeric(date.fine)) / 2)
  
  return(data.fine[findInterval(date.coarse, midpoints)])
} # match.timestep
