##' @name metric.MAE
##' @title Mean Absolute Error
##' @export
##' @param dat dataframe
##' 
##' @author Betsy Cowdery

metric.MAE <- function(dat, ...){
  score <- mean(abs(dat$model - dat$obvs))
  return(score)
}

