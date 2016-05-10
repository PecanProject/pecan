##' @name metric.MSE
##' @title Mean Square Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery

metric.MSE <- function(dat, ...){
  score <- mean((dat$model - dat$obvs)^2, na.rm = TRUE)
  return(score)
}

