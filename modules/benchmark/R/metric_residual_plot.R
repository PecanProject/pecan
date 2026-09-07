##' Residual Plot
##'
##' @param metric_dat dataframe to plot, with at least columns `time`, `model`, `obvs`
##' @param var variable name, used as plot title
##' @param unit measurement unit for the variable, added to the y-axis label; NULL to omit
##' @param filename path to save plot, or NA to not save
##' @param draw.plot logical: Return the plot object?
##' 
##' @author Betsy Cowdery
##' @export
metric_residual_plot <- function(metric_dat, var, unit = NULL, filename = NA, draw.plot = is.na(filename)) {
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
  
  is_multi_site <- "site" %in% colnames(metric_dat) && length(unique(metric_dat$site)) > 1
  is_multi_var <- "variable" %in% colnames(metric_dat) && length(unique(metric_dat$variable)) > 1
  
  if (!"site" %in% colnames(metric_dat)) metric_dat$site <- "All"
  if (!"variable" %in% colnames(metric_dat)) metric_dat$variable <- var
  
  facet_groups <- split(metric_dat, list(metric_dat$site, metric_dat$variable), drop = TRUE)
  
  fit_results <- lapply(names(facet_groups), function(g) {
    sub_dat <- facet_groups[[g]]
    
    time_num <- as.numeric(sub_dat$time)
    if (inherits(sub_dat$time, "POSIXt")) {
      time_num <- time_num / 86400
    }
    time_num <- time_num - min(time_num, na.rm = TRUE)
    sub_dat$time_num <- time_num
    
    fit <- try(stats::lm(diff ~ time_num, data = sub_dat), silent = TRUE)
    if (!inherits(fit, "try-error") && length(stats::coef(fit)) == 2 && !is.na(stats::coef(fit)[2])) {
      intercept <- stats::coef(fit)[1]
      slope <- stats::coef(fit)[2]
      label_str <- sprintf("Intercept: %.3f\nSlope: %.4f / day", intercept, slope)
      sub_dat$trend <- stats::fitted(fit)
    } else {
      label_str <- "Trend: N/A"
      sub_dat$trend <- NA_real_
    }
    
    annot <- data.frame(
      site = sub_dat$site[1],
      variable = sub_dat$variable[1],
      label = label_str
    )
    
    list(sub_dat = sub_dat, annot = annot)
  })
  
  metric_dat <- do.call(rbind, lapply(fit_results, `[[`, "sub_dat"))
  annotations <- do.call(rbind, lapply(fit_results, `[[`, "annot"))
  
  ylab <- if (is.null(unit)) "residual (model - obs)" else sprintf("residual (%s)", unit)
  
  p <- ggplot2::ggplot(data = metric_dat, ggplot2::aes(x = .data$time, y = .data$diff)) +
    ggplot2::geom_hline(yintercept = 0, colour = "#666666", linewidth = 1, linetype = 2) +
    ggplot2::geom_point(size = 2, alpha = 0.7, colour = "#619CFF") +
    ggplot2::geom_line(ggplot2::aes(y = .data$trend), colour = "#FF3333", linetype = "dashed", na.rm = TRUE) +
    ggplot2::labs(title = var, x = "time", y = ylab, colour = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_size = 12)
    
  # Add per-panel annotations
  p <- p + ggplot2::geom_label(
    data = annotations,
    ggplot2::aes(x = -Inf, y = Inf, label = .data$label),
    hjust = -0.05, vjust = 1.1,
    inherit.aes = FALSE,
    alpha = 0.8
  )
  
  # Add facets if applicable
  if (is_multi_site && is_multi_var) {
    p <- p + ggplot2::facet_wrap(~ variable + site, scales = "free_y")
  } else if (is_multi_site) {
    p <- p + ggplot2::facet_wrap(~ site, scales = "free_y")
  } else if (is_multi_var) {
    p <- p + ggplot2::facet_wrap(~ variable, scales = "free_y")
  }
  
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
