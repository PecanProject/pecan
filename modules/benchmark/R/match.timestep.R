##' @name match.timestep 
##' @title Match time step
##' @param date.fine numeric
##' @param data.fine data.frame
##' @param date.coarse numeric
##' @export match.timestep
##' 
##' @author Istem Fer
match.timestep <- function(date_coarse, date_fine, data_fine){
  
  out <- data_fine[findInterval(date_coarse, c(-Inf, head(date_fine, -1)) + c(0, diff(as.numeric(date_fine))/2))]
  
  return(out)
}
