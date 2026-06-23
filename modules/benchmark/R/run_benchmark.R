##' Run a simple benchmark pipeline
##'
##' Takes two validated dataframes, aligns by time,
##' computes metrics, and returns a results table with a plot.
##'
##' @param model_df data.frame with columns: time (POSIXct), value (numeric)
##' @param obs_df   data.frame with columns: time (POSIXct), value (numeric)
##' @param metrics character vector of metrics to compute. Options: "RMSE", "MAE"
##' @param tolerance_secs nearest-neighbor time tolerance in seconds (default 1 hour)
##' @param method alignment method: "nearest" or "interpolate"
##'
##' @return list with: metrics (data.frame), aligned (data.frame), plot (ggplot)
##' @export
##' @author Anshul Jain
run_benchmark <- function(model_df, obs_df,
                          metrics = c("RMSE", "MAE"),
                          tolerance_secs = 3600,
                          method = "nearest") {

  # Stage 1: Validate schema
  bm_validate(model_df, obs_df)

  # Stage 2: Align by time
  aligned <- align_by_time(model_df, obs_df, tolerance_secs = tolerance_secs)

  # Stage 3: Compute metrics via registry
  results <- compute_metrics(aligned, metrics)

  # Stage 4: Plot
  plot <- plot_time_series(aligned)

  list(metrics = results, aligned = aligned, plot = plot)
}

##' Validate benchmark input dataframes
##'
##' @param model_df data.frame with columns: time (POSIXct), value (numeric)
##' @param obs_df   data.frame with columns: time (POSIXct), value (numeric)
##' @return invisible(TRUE)
bm_validate <- function(model_df, obs_df) {
  for (df in list(model_df, obs_df)) {
    if (!"time" %in% names(df))
      PEcAn.logger::logger.severe("Missing required column: 'time'")
    if (!"value" %in% names(df))
      PEcAn.logger::logger.severe("Missing required column: 'value'")
      
    if (!inherits(df$time, "POSIXct"))
      PEcAn.logger::logger.severe(paste0("Column 'time' must be POSIXct, got: ", class(df$time)[1]))
    if (!is.numeric(df$value))
      PEcAn.logger::logger.severe(paste0("Column 'value' must be numeric, got: ", class(df$value)[1]))
  }
  invisible(TRUE)
}

##' Align model and observation data frames by nearest time
##'
##' @param model_df data.frame with columns: time (POSIXct), value
##' @param obs_df   data.frame with columns: time (POSIXct), value
##' @param tolerance_secs max allowed time difference in seconds
##'
##' @return data.frame with columns: model, obvs, time
align_by_time <- function(model_df, obs_df, tolerance_secs = 3600) {
  # Sort both dataframes by time to ensure findInterval works correctly
  model_df <- model_df[order(model_df$time), ]
  obs_df <- obs_df[order(obs_df$time), ]
  
  # For each model time, find the interval in obs_time it falls into
  idx <- findInterval(model_df$time, obs_df$time, all.inside = TRUE)
  
  # findInterval returns index i where obs[i] <= model_time < obs[i+1]
  # We check both i and i+1 to see which one is the absolute nearest
  idx_next <- pmin(idx + 1, nrow(obs_df))
  
  diff_current <- abs(as.numeric(difftime(model_df$time, obs_df$time[idx], units = "secs")))
  diff_next <- abs(as.numeric(difftime(model_df$time, obs_df$time[idx_next], units = "secs")))
  
  # Select the index of the closest observation
  nearest_idx <- ifelse(diff_current <= diff_next, idx, idx_next)
  time_diffs <- pmin(diff_current, diff_next)
  
  # Filter by our time tolerance
  valid <- time_diffs <= tolerance_secs
  
  n_kept <- sum(valid)
  n_dropped <- length(valid) - n_kept
  PEcAn.logger::logger.info(sprintf("Time alignment kept %d points and dropped %d points outside of tolerance (%d secs)", n_kept, n_dropped, tolerance_secs))
  
  # Construct the aligned base data.frame
  aligned <- data.frame(
    model = model_df$value[valid],
    obvs = obs_df$value[nearest_idx][valid],
    time = model_df$time[valid]
  )
  
  return(aligned)
}

##' Metric Registry for PEcAn.benchmark
##' @export
pecan_metric_registry <- new.env(parent = emptyenv())

##' Register a new benchmark metric
##'
##' @param name Character name of the metric
##' @param func Function that takes an aligned dataframe and returns a numeric value
##' @export
register_metric <- function(name, func) {
  assign(toupper(name), func, envir = pecan_metric_registry)
}

# Pre-populate default metrics
register_metric("RMSE", function(dat) sqrt(mean((dat$model - dat$obvs)^2, na.rm = TRUE)))
register_metric("MAE",  function(dat) mean(abs(dat$model - dat$obvs), na.rm = TRUE))
register_metric("R2",   function(dat) {
  if (exists("metric_R2", where = asNamespace("PEcAn.benchmark"), mode = "function")) {
    return(PEcAn.benchmark::metric_R2(dat))
  }
  numer <- sum((dat$obvs - mean(dat$obvs, na.rm=T)) * (dat$model - mean(dat$model, na.rm=T)), na.rm=T)
  denom <- sqrt(sum((dat$obvs - mean(dat$obvs, na.rm=T))^2, na.rm=T)) * sqrt(sum((dat$model - mean(dat$model, na.rm=T))^2, na.rm=T))
  (numer / denom)^2
})
register_metric("NSE",  function(dat) {
  # Nash-Sutcliffe Efficiency
  1 - (sum((dat$obvs - dat$model)^2, na.rm = TRUE) / sum((dat$obvs - mean(dat$obvs, na.rm = TRUE))^2, na.rm = TRUE))
})
register_metric("MEF", get("NSE", envir = pecan_metric_registry))

##' Compute benchmark metrics
##'
##' @param aligned data.frame with columns: model, obvs, time
##' @param metrics character vector of metric names
##' @return data.frame with columns: metric, value
compute_metrics <- function(aligned, metrics = c("RMSE", "MAE", "R2")) {
  results <- lapply(toupper(metrics), function(m) {
    if (!exists(m, envir = pecan_metric_registry)) {
      PEcAn.logger::logger.severe(paste0("Unknown metric: ", m))
    }
    func <- get(m, envir = pecan_metric_registry)
    func(aligned)
  })
  
  data.frame(metric = toupper(metrics), value = unlist(results, use.names = FALSE))
}

##' Plot model vs observations time series
##'
##' @param aligned data.frame with columns: model, obvs, time
##' @return ggplot object
plot_time_series <- function(aligned) {
  ggplot2::ggplot(aligned, ggplot2::aes(x = .data$time)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$model, color = "Model")) +
    ggplot2::geom_point(ggplot2::aes(y = .data$obvs, color = "Obs")) +
    ggplot2::labs(color = "", y = "value", title = "Model vs Observations") +
    ggplot2::theme_bw()
}
