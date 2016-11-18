##' @name metric_timeseries_plot
##' @title Timeseries Plot
##' @export
##' @param metric_dat dataframe
##' @param var character
##' 
##' @author Betsy Cowdery

metric_timeseries_plot <- function(metric_dat, var, filename = NA, draw.plot = FALSE) {
  logger.info("Metric: Timeseries Plot")
  
  # Attempt at getting around the fact that time can be annual and thus as.Date won't work
  date.time <- try(as.Date(metric_dat$time), silent = TRUE)
  if (class(date.time) == "try-error"){
    logger.warn("Can't coerce time column to Date format, attempting plot anyway")
  }else{
    metric_dat$time <- date.time
  }
  
  p <- ggplot2::ggplot(data = metric_dat, aes(x = time)) 
  p <- p + ggplot2::labs(title = var, y = "") 
  p <- p + ggplot2::geom_path(aes(y = model, colour = "Model"), size = 2) 
  p <- p + ggplot2::geom_point(aes(y = model, colour = "Model"), size = 4) 
  p <- p + ggplot2::geom_path(aes(y = obvs, colour = "Observed"), size = 2) 
  p <- p + ggplot2::geom_point(aes(y = obvs, colour = "Observed"), size = 4)
  
  if (!is.na(filename)) {
    pdf(filename, width = 10, height = 6)
    plot(p)
    dev.off()
  }
  
  if (draw.plot) {
    plot(p)
  }
} # metric_timeseries_plot
