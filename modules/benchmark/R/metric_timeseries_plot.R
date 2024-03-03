##' Timeseries Plot
##'
##' @param metric_dat dataframe to plot, with at least columns `time`, `model`, `obvs`
##' @param var variable name, used as plot title
##' @param filename path to save plot, or NA to not save
##' @param draw.plot logical: Return the plot object?
##'
##' @author Betsy Cowdery
##' @importFrom ggplot2 ggplot labs geom_path geom_point
##' @export

metric_timeseries_plot <- function(metric_dat, var, filename = NA, draw.plot = is.na(filename)) {
  PEcAn.logger::logger.info("Metric: Timeseries Plot")
  
  # Attempt at getting around the fact that time can be annual and thus as.Date won't work
  date.time <- try(as.Date(metric_dat$time), silent = TRUE)
  if (inherits(date.time, "try-error")) {
    PEcAn.logger::logger.warn("Can't coerce time column to Date format, attempting plot anyway")
  }else{
    metric_dat$time <- date.time
  }
  
  p <- ggplot(data = metric_dat, ggplot2::aes(x = .data$time)) 
  p <- p + labs(title = var, y = "") 
  p <- p + geom_path(ggplot2::aes(y = .data$model, colour = "Model"), size = 2) 
  p <- p + geom_point(ggplot2::aes(y = .data$model, colour = "Model"), size = 4) 
  p <- p + geom_path(ggplot2::aes(y = .data$obvs, colour = "Observed"), size = 2) 
  p <- p + geom_point(ggplot2::aes(y = .data$obvs, colour = "Observed"), size = 4)
  
  if (!is.na(filename)) {
    grDevices::pdf(filename, width = 10, height = 6)
    plot(p)
    grDevices::dev.off()
  }
  
  if (draw.plot) {
    return(p)
  }
} # metric_timeseries_plot
