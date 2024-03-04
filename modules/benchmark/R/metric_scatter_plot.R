##' Scatter Plot
##'
##' @param metric_dat dataframe to plot, with at least columns `model` and `obvs`
##' @param var ignored
##' @param filename path to save plot, or NA to not save
##' @param draw.plot logical: Return the plot object?
##' 
##' @author Betsy Cowdery
##' @export

metric_scatter_plot <- function(metric_dat, var, filename = NA, draw.plot = is.na(filename)) {
  PEcAn.logger::logger.info("Metric: Scatter Plot")
  
  p <- ggplot2::ggplot(data = metric_dat) 
  p <- p + ggplot2::geom_point(ggplot2::aes(x = .data$model, y = .data$obvs), size = 4) 
  p <- p + ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#666666", 
                       size = 2, linetype = 2)
  
  if (!is.na(filename)) {
    grDevices::pdf(filename, width = 10, height = 6)
    plot(p)
    grDevices::dev.off()
  }
  
  if (draw.plot) {
    return(p)
  }

} # metric_scatter_plot
