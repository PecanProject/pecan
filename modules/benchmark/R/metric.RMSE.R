##' @name metric.RMSE
##' @title Root Mean Square Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery

metric.RMSE <- function(dat, ...){
  score <- sqrt(mean((dat$model - dat$obvs)^2, na.rm = TRUE))
  return(score)
}

