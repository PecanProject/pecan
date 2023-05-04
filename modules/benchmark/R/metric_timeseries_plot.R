##' @name metric_timeseries_plot
##' @title Timeseries Plot
##' @export
##' @param metric_dat
##' @param var
##' @param filename
##' @param draw.plot
##' @importFrom ggplot2 ggplot labs geom_path geom_point
##' @author Betsy Cowdery

metric_timeseries_plot <- function(metric_dat, var, filename = NA, draw.plot = is.na(filename)) {
  PEcAn.logger::logger.info("Metric: Timeseries Plot")
  
  # Attempt at getting around the fact that time can be annual and thus as.Date won't work
  date.time <- try(as.Date(metric_dat$time), silent = TRUE)
  if (inherits(date.time, "try-error")) {
    PEcAn.logger::logger.warn("Can't coerce time column to Date format, attempting plot anyway")
  }else{
    metric_dat$time <- date.time
  }
  
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
    return(p)
  }
} # metric_timeseries_plot
