##' @name metric.timeseries.plot
##' @title metric.timeseries.plot
##' @export
##' @param metric_dat dataframe
##' @param var character
##' 
##' @author Betsy Cowdery
metric.timeseries.plot <- function(metric_dat, var, filename = NA, draw.plot = FALSE) {
  
  library(ggplot2)
  
  localenv <- environment()
  
  # Attempt at getting around the fact that time can be annual and thus as.Date won't work
  date.time <- try(as.Date(metric_dat$time))
  if (class(date.time) == "try-error"){
    logger.warn("Can't coerce time column to Date format, attempting plot anyway")
  }else{
    metric_dat$time <- date.time
  }
  
  # p <- ggplot(data = metric_dat, aes(x=time)) + 
  #   geom_path(aes(y=model),colour = "#666666", size=2) +
  #   geom_point(aes(y=model),colour = "#666666", size=4) +
  #   geom_path(aes(y=obvs), colour = "#619CFF", size=2) + 
  #   geom_point(aes(y=obvs), colour = "#619CFF", size=4) +
  #   labs(title=var, y="")
  
  p <- ggplot(data = metric_dat, aes(x = time)) 
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
