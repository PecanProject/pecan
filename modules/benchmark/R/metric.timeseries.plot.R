##' @name metric.timeseries.plot
##' @title metric.timeseries.plot
##' @export
##' @param dat dataframe
##' @param var character
##' 
##' @author Betsy Cowdery
metric.timeseries.plot <- function(dat, var, filename = NA, draw.plot = FALSE) {
  
  library(ggplot2)
  
  localenv <- environment()
  dat$time <- as.Date(dat$time)
  
  # p <- ggplot(data = dat, aes(x=time)) + 
  #   geom_path(aes(y=model),colour = "#666666", size=2) +
  #   geom_point(aes(y=model),colour = "#666666", size=4) +
  #   geom_path(aes(y=obvs), colour = "#619CFF", size=2) + 
  #   geom_point(aes(y=obvs), colour = "#619CFF", size=4) +
  #   labs(title=var, y="")
  
  p <- ggplot(data = dat, aes(x = time)) 
  p <- p + labs(title = var, y = "") 
  p <- p + geom_path(aes(y = model, colour = "Model"), size = 2) 
  p <- p + geom_point(aes(y = model, colour = "Model"), size = 4) 
  p <- p + geom_path(aes(y = obvs, colour = "Observed"), size = 2) 
  p <- p + geom_point(aes(y = obvs, colour = "Observed"), size = 4)
  
  if (!is.na(filename)) {
    pdf(filename, width = 10, height = 6)
    plot(p)
    dev.off()
  }
  
  if (draw.plot) {
    plot(p)
  }
} # metric.timeseries.plot
