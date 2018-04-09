##' @name metric_residual_plot
##' @title Residual Plot
##' @export
##' @param metric_dat
##' @param var
##' @param filename
##' @param draw.plot
##' 
##' @author Betsy Cowdery
metric_residual_plot <- function(metric_dat, var, filename = NA, draw.plot = is.na(filename)) {
  PEcAn.logger::logger.info("Metric: Residual Plot")
  
  metric_dat$time <- lubridate::year(as.Date(as.character(metric_dat$time), format = "%Y"))
  metric_dat$diff <- abs(metric_dat$model - metric_dat$obvs)
  metric_dat$zeros <- rep(0, length(time))
  
  p <- ggplot2::ggplot(data = metric_dat, aes(x = time)) 
  p <- p + ggplot2::geom_path(aes(y = zeros), colour = "#666666", size = 2, linetype = 2, lineend = "round") 
  p <- p + ggplot2::geom_point(aes(y = diff), size = 4, colour = "#619CFF") 
  p <- p + ggplot2::labs(title = var, x = "years", y = "abs(model - observation)")
  
  if (!is.na(filename)) {
    pdf(filename, width = 10, height = 6)
    plot(p)
    dev.off()
  }
  
  if (draw.plot) {
    return(p)
  }
} # metric_residual_plot