##' @name metric.timeseries.plot
##' @title metric.timeseries.plot
##' @export
##' @param dat
##' 
##' @author Betsy Cowdery

metric.timeseries.plot <- function(dat, var){
  
  require(ggplot2)
  
  dat$time <- as.Date(dat$time)
  
  ggplot(data = dat, aes(x=time)) + 
    geom_path(aes(y=model),colour = "#666666", size=2) +
    geom_point(aes(y=model),colour = "#666666", size=4) +
    geom_path(aes(y=obvs), colour = "#619CFF", size=2) + 
    geom_point(aes(y=obvs), colour = "#619CFF", size=4) +
    labs(title=var)
  
  return(NA)
}