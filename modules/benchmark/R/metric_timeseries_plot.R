##' Timeseries Plot
##'
##' @param metric_dat dataframe to plot, with at least columns `time`, `model`, `obvs`
##' @param var variable name, used as plot title
##' @param unit measurement unit for the variable, added to the y-axis label; NULL to omit
##' @param filename path to save plot, or NA to not save
##' @param draw.plot logical: Return the plot object?
##'
##' @author Betsy Cowdery
##' @export
metric_timeseries_plot <- function(metric_dat, var, unit = NULL, filename = NA, draw.plot = is.na(filename)) {
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
  
  if (!"model_q05" %in% colnames(metric_dat)) metric_dat$model_q05 <- metric_dat$model
  if (!"model_q95" %in% colnames(metric_dat)) metric_dat$model_q95 <- metric_dat$model

  is_multi_site <- "site" %in% colnames(metric_dat) && length(unique(metric_dat$site)) > 1
  is_multi_var <- "variable" %in% colnames(metric_dat) && length(unique(metric_dat$variable)) > 1
  
  if (!"site" %in% colnames(metric_dat)) metric_dat$site <- "All"
  if (!"variable" %in% colnames(metric_dat)) metric_dat$variable <- var

  # Calculate Scores per facet
  facet_groups <- split(metric_dat, list(metric_dat$site, metric_dat$variable), drop = TRUE)
  
  annotations <- do.call(rbind, lapply(names(facet_groups), function(g) {
    sub_dat <- facet_groups[[g]]
    
    coverage_val <- try(metric_Coverage(sub_dat), silent = TRUE)
    if (inherits(coverage_val, "try-error") || is.na(coverage_val)) {
      coverage_pct <- NA_real_
    } else {
      coverage_pct <- coverage_val * 100
    }
    
    sharpness <- mean(sub_dat$model_q95 - sub_dat$model_q05, na.rm = TRUE)
    bias <- mean(sub_dat$model - sub_dat$obvs, na.rm = TRUE)
    
    pmu_val_str <- "N/A"
    pass_str <- "N/A"
    if ("obs_se" %in% colnames(sub_dat) && "obs_n" %in% colnames(sub_dat)) {
      valid_pmu <- !is.na(sub_dat$obs_se) & !is.na(sub_dat$obs_n)
      if (any(valid_pmu)) {
        se2_n <- (sub_dat$obs_se[valid_pmu]^2) * sub_dat$obs_n[valid_pmu]
        pooled_var <- sum(se2_n) / sum(sub_dat$obs_n[valid_pmu])
        pmu <- sqrt(pooled_var)
        pmu_val_str <- sprintf("%.2f", pmu)
        
        passes_validation <- (!is.na(coverage_pct) && coverage_pct >= 90) && (abs(bias) < pmu)
        pass_str <- ifelse(passes_validation, "PASS", "FAIL")
      }
    }
    
    data.frame(
      site = sub_dat$site[1],
      variable = sub_dat$variable[1],
      label = sprintf("Coverage: %.1f%%\nSharpness: %.2f\nBias: %.2f\nPMU: %s\nStatus: %s", coverage_pct, sharpness, bias, pmu_val_str, pass_str)
    )
  }))

  # Determine if model "passes" at each point based on available intervals
  metric_dat$Pass <- metric_dat$obvs >= metric_dat$model_q05 & metric_dat$obvs <= metric_dat$model_q95

  if (any(!is.na(metric_dat$Pass))) {
    metric_dat$Observation_Status <- ifelse(metric_dat$Pass, "Observed (Pass)", "Observed (Fail)")
  } else {
    metric_dat$Observation_Status <- "Observed"
  }

  p <- ggplot2::ggplot(data = metric_dat, ggplot2::aes(x = .data$time))
  
  # 1. Model Ribbon
  p <- p + 
    ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$model_q05, ymax = .data$model_q95, fill = "Model 90% CI"), alpha = 0.3) +
    ggplot2::scale_fill_manual(values = c("Model 90% CI" = "#619CFF"), name = "Intervals")
  
  # 2. Model Mean Line
  p <- p + ggplot2::geom_line(ggplot2::aes(y = .data$model, colour = "Model"), linewidth = 1)

  # 3. Observational Points & Error Bars
  if ("obvs_sd" %in% colnames(metric_dat)) {
    p <- p + ggplot2::geom_pointrange(
      ggplot2::aes(y = .data$obvs, ymin = .data$obvs - .data$obvs_sd, ymax = .data$obvs + .data$obvs_sd, colour = .data$Observation_Status), 
      size = 0.5, alpha = 0.7
    )
  } else {
    p <- p + ggplot2::geom_point(ggplot2::aes(y = .data$obvs, colour = .data$Observation_Status), size = 2, alpha = 0.7)
  }
  
  ylab <- if (is.null(unit)) "value" else sprintf("%s (%s)", tolower(var), unit)
  
  # Adjust Scales
  p <- p +
    ggplot2::scale_colour_manual(values = c(
      "Model" = "#619CFF", 
      "Observed" = "#F8766D", 
      "Observed (Pass)" = "#00BA38", 
      "Observed (Fail)" = "#F8766D"
    )) +
    ggplot2::labs(title = var, x = "time", y = ylab, colour = NULL, fill = NULL) +
    ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(legend.position = "bottom")

  # Add per-panel annotations
  p <- p + ggplot2::geom_label(
    data = annotations,
    ggplot2::aes(x = -Inf, y = Inf, label = label),
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
} # metric_timeseries_plot
