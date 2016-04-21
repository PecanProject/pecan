##' @name metric.RMSE
##' @title metric.RMSE
##' @export
##' @param dat
##' 
##' @author Betsy Cowdery

metric.RMSE <- function(dat, ...){
  score <- sqrt(mean((dat$model - dat$obvs)^2, na.rm = TRUE))
  return(score)
}

