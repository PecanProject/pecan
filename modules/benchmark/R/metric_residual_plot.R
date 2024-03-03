##' Residual Plot
##'
##' @param metric_dat dataframe to plot, with at least columns `time`, `model`, `obvs`
##' @param var variable name, used as plot title
##' @param filename path to save plot, or NA to not save
##' @param draw.plot logical: Return the plot object?
##' 
##' @author Betsy Cowdery
##' @export
metric_residual_plot <- function(metric_dat, var, filename = NA, draw.plot = is.na(filename)) {
  PEcAn.logger::logger.info("Metric: Residual Plot")
  
  metric_dat$time <- lubridate::year(as.Date(as.character(metric_dat$time), format = "%Y"))
  metric_dat$diff <- abs(metric_dat$model - metric_dat$obvs)
  metric_dat$zeros <- rep(0, length(metric_dat$time))
  
  p <- ggplot2::ggplot(data = metric_dat, ggplot2::aes(x = .data$time)) 
  p <- p + ggplot2::geom_path(ggplot2::aes(y = .data$zeros), colour = "#666666", size = 2, linetype = 2, lineend = "round") 
  p <- p + ggplot2::geom_point(ggplot2::aes(y = .data$diff), size = 4, colour = "#619CFF") 
  p <- p + ggplot2::labs(title = var, x = "years", y = "abs(model - observation)")
  
  if (!is.na(filename)) {
    grDevices::pdf(filename, width = 10, height = 6)
    plot(p)
    grDevices::dev.off()
  }
  
  if (draw.plot) {
    return(p)
  }
} # metric_residual_plot