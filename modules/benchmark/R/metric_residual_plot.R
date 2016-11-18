##' @name metric_residual_plot
##' @title Residual Plot
##' @export
##' @param dat
##' 
##' @author Betsy Cowdery
metric_residual_plot <- function(dat, var, filename = NA, draw.plot = FALSE) {
  logger.info("Metric: Residual Plot")
  
  dat$time <- lubridate::year(as.Date(as.character(dat$time), format = "%Y"))
  dat$diff <- abs(dat$model - dat$obvs)
  dat$zeros <- rep(0, length(time))
  
  p <- ggplot2::ggplot(data = dat, aes(x = time)) 
  p <- p + ggplot2::geom_path(aes(y = zeros), colour = "#666666", size = 2, linetype = 2, lineend = "round") 
  p <- p + ggplot2::geom_point(aes(y = diff), size = 4, colour = "#619CFF") 
  p <- p + ggplot2::labs(title = var, x = "years", y = "abs(model - observation)")
  
  if (!is.na(filename)) {
    pdf(filename, width = 10, height = 6)
    plot(p)
    dev.off()
  }
  
  if (draw.plot) {
    plot(p)
  }
} # metric_residual_plot