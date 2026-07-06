##' Timeseries Plot
##'
##' @param metric_dat dataframe to plot, with at least columns `time`, `model`, `obvs`
##' @param var variable name, used as plot title
##' @param filename path to save plot, or NA to not save
##' @param draw.plot logical: Return the plot object?
##'
##' @author Betsy Cowdery
##' @export
metric_timeseries_plot <- function(metric_dat, var, filename = NA, draw.plot = is.na(filename)) {
  PEcAn.logger::logger.info("Metric: Timeseries Plot")
  
  # Ensure metric_dat is a data.frame for ggplot2
  metric_dat <- as.data.frame(metric_dat)
  
  if (!"time" %in% colnames(metric_dat)) {
    PEcAn.logger::logger.warn("Missing 'time' column in metric_dat, using row index instead.")
    metric_dat$time <- seq_len(nrow(metric_dat))
  } else {
    date.time <- try(as.Date(as.character(metric_dat$time)), silent = TRUE)
    if (!inherits(date.time, "try-error") && !all(is.na(date.time))) {
      metric_dat$time <- date.time
    } else {
      PEcAn.logger::logger.warn("Can't coerce time column to Date format, using original format.")
    }
  }
  
  p <- ggplot2::ggplot(data = metric_dat, ggplot2::aes(x = .data$time))
  
  # 1. Model Ribbon (if available)
  if (all(c("model_q05", "model_q95") %in% colnames(metric_dat))) {
    p <- p + ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$model_q05, ymax = .data$model_q95, fill = "Model 90% CI"), alpha = 0.3)
  }
  
  # 2. Model Mean Line
  p <- p + ggplot2::geom_line(ggplot2::aes(y = .data$model, colour = "Model"), linewidth = 1)
  
  # 3. Observational Points & Error Bars
  if ("obvs_sd" %in% colnames(metric_dat)) {
    p <- p + ggplot2::geom_pointrange(
      ggplot2::aes(y = .data$obvs, ymin = .data$obvs - .data$obvs_sd, ymax = .data$obvs + .data$obvs_sd, colour = "Observed"), 
      size = 0.5, alpha = 0.7
    )
  } else {
    p <- p + ggplot2::geom_point(ggplot2::aes(y = .data$obvs, colour = "Observed"), size = 2, alpha = 0.7)
  }
  
  # Adjust Scales
  p <- p +
    ggplot2::scale_colour_manual(values = c("Model" = "#619CFF", "Observed" = "#F8766D")) +
    ggplot2::scale_fill_manual(values = c("Model 90% CI" = "#619CFF"), name = "Intervals") +
    ggplot2::labs(title = var, x = "Time", y = "Value", color = "Source") +
    ggplot2::theme_minimal(base_size = 14) +
    ggplot2::theme(legend.position = "bottom")
  
  if (!is.na(filename)) {
    grDevices::pdf(filename, width = 10, height = 6)
    print(p)
    grDevices::dev.off()
  }
  
  if (draw.plot) {
    return(p)
  }
  invisible(p)
} # metric_timeseries_plot
