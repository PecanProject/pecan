##' @name match.timestep 
##' @title Match time step
##' @param date.fine numeric
##' @param data.fine matrix
##' @param date.coarse numeric
##' @export match.timestep
##' 
##' @author Istem Fer
match.timestep <- function(date_coarse, date_fine, data_fine) {
  
  midpoints <- c(-Inf, head(date_fine, -1)) + c(0, diff(as.numeric(date_fine)) / 2)
  
  data_fine[findInterval(date_coarse, midpoints)]
} # match.timestep
