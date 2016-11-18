##' @name metric_scatter_plot
##' @title Scatter Plot
##' @export
##' @param metric_dat
##' @param var
##' 
##' @author Betsy Cowdery

metric_scatter_plot <- function(metric_dat, var, filename = NA, draw.plot = FALSE) {
  logger.info("Metric: Scatter Plot")
  
  p <- ggplot2::ggplot(data = metric_dat) 
  p <- p + ggplot2::geom_point(aes(x = model, y = obvs), size = 4) 
  p <- p + ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#666666", 
                       size = 2, linetype = 2)
  
  if (!is.na(filename)) {
    pdf(filename, width = 10, height = 6)
    plot(p)
    dev.off()
  }
  
  if (draw.plot) {
    plot(p)
  }

} # metric_scatter_plot
