##' @name mean.over.larger.timestep 
##' @title Calculate benchmarking statistics
##' @param date.fine numeric
##' @param data.fine data.frame
##' @param date.coarse numeric
##' @export 
##' 
##' @author Betsy Cowdery, Michael Dietze
mean.over.larger.timestep <- function(date.coarse, date.fine, data.fine){
  tapply(X=data.fine, INDEX=findInterval(as.Date(date.fine), as.Date(date.coarse)), FUN=mean)
}
