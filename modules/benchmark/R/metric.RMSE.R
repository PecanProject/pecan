##' @name metric.RMSE
##' @title metric.RMSE
##' @export
##' @param data.path
##' @param format
##' 
##' @author Betsy Cowdery

metric.RMSE <- function(dat){
  score <- sqrt(mean((dat$obvs - dat$model)^2))
  return(score)
}

