##' Scatter Plot
##'
##' @param metric_dat dataframe to plot, with at least columns `model` and `obvs`
##' @param var title for the plot
##' @param filename path to save plot, or NA to not save
##' @param draw.plot logical: Return the plot object?
##' 
##' @author Betsy Cowdery
##' @export
metric_scatter_plot <- function(metric_dat, var, filename = NA, draw.plot = is.na(filename)) {
  PEcAn.logger::logger.info("Metric: Scatter Plot")
  
  metric_dat <- as.data.frame(metric_dat)
  
  p <- ggplot2::ggplot(data = metric_dat, ggplot2::aes(x = .data$model, y = .data$obvs)) +
    ggplot2::geom_point(size = 2, alpha = 0.7, colour = "#619CFF") +
    ggplot2::geom_abline(slope = 1, intercept = 0, colour = "#666666", 
                         linewidth = 1, linetype = 2) +
    ggplot2::labs(title = var, x = "Modeled", y = "Observed") +
    ggplot2::theme_minimal(base_size = 14)
  
  if (!is.na(filename)) {
    grDevices::pdf(filename, width = 10, height = 6)
    print(p)
    grDevices::dev.off()
  }
  
  if (draw.plot) {
    return(p)
  }
  invisible(p)
} # metric_scatter_plot
