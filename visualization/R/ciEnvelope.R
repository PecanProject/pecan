#' plots a confidence interval around an x-y plot (e.g. a timeseries)
#' 
#' @param x Vector defining CI center
#' @param ylo Vector defining bottom of CI envelope
#' @param yhi Vector defining top of CI envelope
#' @export 
#' @author Michael Dietze, David LeBauer
ciEnvelope <- function(x,ylo,yhi,...){
  polygon(cbind(c(x, rev(x), x[1]), c(ylo, rev(yhi),
                                      ylo[1])), border = NA,...) 
}
