##' @name metric.scatter.plot
##' @title metric.scatter.plot
##' @export
##' @param dat
##' 
##' @author Betsy Cowdery

metric.scatter.plot <- function(dat, var){
  
  library(ggplot2)
  
  ggplot(data = dat) + 
    geom_point(aes(x=model,y=obvs), size=4) + 
    geom_abline(slope=1,intercept=0, colour = "#666666", size=2, linetype = 2, lineend = "round")  
  
  return(NA)
}

