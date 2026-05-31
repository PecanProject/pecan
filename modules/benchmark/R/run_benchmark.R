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
    if (!inherits(df$time, "POSIXct"))
      stop("Column 'time' must be POSIXct, got: ", class(df$time))
    if (!is.numeric(df$value))
      stop("Column 'value' must be numeric, got: ", class(df$value))
  }
  invisible(TRUE)
}

##' Align model and observation data frames by nearest time
##'
##' @param model_df data.frame with columns: time (POSIXct), value
##' @param obs_df   data.frame with columns: time (POSIXct), value
##' @param tolerance_secs max allowed time difference in seconds
##'
##' @return data.frame with columns: time, model, obs
align_by_time <- function(model_df, obs_df, tolerance_secs = 3600) {
  # Ensure data.table is available
  if (!requireNamespace("data.table", quietly = TRUE)) {
    stop("Package 'data.table' is required for high-performance alignment.")
  }
  
  # Convert to data.table and explicitly name value columns
  dt_model <- data.table::data.table(time = model_df$time, model = model_df$value)
  # Keep original obs time in a separate column to calculate difference after join
  dt_obs <- data.table::data.table(time = obs_df$time, obs = obs_df$value, obs_time = obs_df$time)
  
  # Set keys for fast roll-join
  data.table::setkey(dt_model, time)
  data.table::setkey(dt_obs, time)
  
  # Perform rolling join to nearest time
  aligned <- dt_obs[dt_model, roll = "nearest"]
  
  # Filter by tolerance
  aligned[, time_diff := abs(as.numeric(difftime(obs_time, time, units = "secs")))]
  aligned <- aligned[time_diff <= tolerance_secs]
  
  # Format output to match expected schema
  aligned <- aligned[, .(time, model, obs)]
  data.table::setDF(aligned) # Convert back to base data.frame
  
  return(aligned)
}

##' Compute benchmark metrics
##'
##' @param aligned data.frame with columns: time, model, obs
##' @param metrics character vector of metric names
##' @return data.frame with columns: metric, value
compute_metrics <- function(aligned, metrics = c("RMSE", "MAE", "R2")) {
  # Future-proofing: Functions in the registry now accept the full aligned dataframe
  # This aligns with the decoupled metric architecture introduced in PR #3888
  METRIC_REGISTRY <- list(
    RMSE = function(dat) sqrt(mean((dat$model - dat$obs)^2, na.rm = TRUE)),
    MAE  = function(dat) mean(abs(dat$model - dat$obs), na.rm = TRUE),
    R2   = function(dat) {
      if (exists("metric_R2", where = asNamespace("PEcAn.benchmark"), mode = "function")) {
        return(PEcAn.benchmark::metric_R2(dat))
      }
      # Fallback if PR #3888 is not yet merged
      numer <- sum((dat$obs - mean(dat$obs, na.rm=T)) * (dat$model - mean(dat$model, na.rm=T)), na.rm=T)
      denom <- sqrt(sum((dat$obs - mean(dat$obs, na.rm=T))^2, na.rm=T)) * sqrt(sum((dat$model - mean(dat$model, na.rm=T))^2, na.rm=T))
      (numer / denom)^2
    }
  )
  
  results <- lapply(toupper(metrics), function(m) {
    if (!m %in% names(METRIC_REGISTRY)) stop("Unknown metric: ", m)
    METRIC_REGISTRY[[m]](aligned)
  })
  
  data.frame(metric = toupper(metrics), value = unlist(results, use.names = FALSE))
}

##' Plot model vs observations time series
##'
##' @param aligned data.frame with columns: time, model, obs
##' @return ggplot object
plot_time_series <- function(aligned) {
  ggplot2::ggplot(aligned, ggplot2::aes(x = .data$time)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$model, color = "Model")) +
    ggplot2::geom_line(ggplot2::aes(y = .data$obs, color = "Obs")) +
    ggplot2::labs(color = "", y = "value", title = "Model vs Observations") +
    ggplot2::theme_bw()
}
