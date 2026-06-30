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
  
  metric_dat <- as.data.frame(metric_dat)
  
  if (!"time" %in% colnames(metric_dat)) {
    metric_dat$time <- seq_len(nrow(metric_dat))
  } else {
    date.time <- try(as.Date(as.character(metric_dat$time)), silent = TRUE)
    if (!inherits(date.time, "try-error") && !all(is.na(date.time))) {
      metric_dat$time <- date.time
    }
  }
  
  # Calculate residuals (Model - Observation)
  metric_dat$diff <- metric_dat$model - metric_dat$obvs
  
  p <- ggplot2::ggplot(data = metric_dat, ggplot2::aes(x = .data$time, y = .data$diff)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#666666", linewidth = 1, linetype = 2) +
    ggplot2::geom_point(size = 2, alpha = 0.7, colour = "#619CFF") +
    ggplot2::labs(title = var, x = "Time", y = "Residual (Model - Obs)") +
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
} # metric_residual_plot