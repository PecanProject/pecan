##' @name metric.timeseries.plot
##' @title metric.timeseries.plot
##' @export
##' @param dat
##' 
##' @author Betsy Cowdery

metric.timeseries.plot <- function(dat, var){
  
  require(ggplot2)
  
  ggplot(data = dat) + 
    geom_path(aes(x=time,y=model),colour = "#666666", size=2) +
    geom_point(aes(x=time,y=model),colour = "#666666", size=4) +
    geom_path(aes(x=time,y=obvs), colour = "#619CFF", size=2) + 
    geom_point(aes(x=time,y=obvs), colour = "#619CFF", size=4)+ 
    labs(title=var)
  
  return(NA)
}