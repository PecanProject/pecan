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
  aligned <- align_by_time(model_df, obs_df,
                           tolerance_secs = tolerance_secs,
                           method = method)

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
##' Uses a binary-search nearest join (O(N log N)) to match each model
##' timestep to the closest observation within \code{tolerance_secs}.
##' Only the "nearest" method is currently implemented; the \code{method}
##' argument is accepted for forward compatibility.
##'
##' @param model_df data.frame with columns: time (POSIXct), value
##' @param obs_df   data.frame with columns: time (POSIXct), value
##' @param tolerance_secs max allowed time difference in seconds
##' @param method alignment method: currently only "nearest" is supported
##'
##' @return data.frame with columns: time, model, obs
align_by_time <- function(model_df, obs_df, tolerance_secs = 3600,
                          method = "nearest") {
  t_model <- as.numeric(model_df$time)
  t_obs   <- as.numeric(obs_df$time)

  obs_ord        <- order(t_obs)
  t_obs_sorted   <- t_obs[obs_ord]
  obs_val_sorted <- obs_df$value[obs_ord]

  # findInterval gives the index of the last obs <= each model time (binary search)
  idx_lo <- findInterval(t_model, t_obs_sorted)

  nearest_idx <- vapply(seq_along(t_model), function(i) {
    lo <- idx_lo[i]
    hi <- lo + 1L
    if (lo < 1L) return(hi)
    if (hi > length(t_obs_sorted)) return(lo)
    if (abs(t_obs_sorted[hi] - t_model[i]) < abs(t_obs_sorted[lo] - t_model[i])) hi else lo
  }, integer(1L))

  diffs <- abs(t_obs_sorted[nearest_idx] - t_model)
  keep  <- diffs <= tolerance_secs

  data.frame(
    time  = model_df$time[keep],
    model = model_df$value[keep],
    obs   = obs_val_sorted[nearest_idx[keep]]
  )
}

##' Compute benchmark metrics
##'
##' @param aligned data.frame with columns: time, model, obs
##' @param metrics character vector of metric names
##' @return data.frame with columns: metric, value
compute_metrics <- function(aligned, metrics = c("RMSE", "MAE")) {
  METRIC_REGISTRY <- list(
    RMSE = function(x, y) sqrt(mean((x - y)^2, na.rm = TRUE)),
    MAE  = function(x, y) mean(abs(x - y), na.rm = TRUE)
  )
  results <- lapply(toupper(metrics), function(m) {
    if (!m %in% names(METRIC_REGISTRY)) stop("Unknown metric: ", m)
    METRIC_REGISTRY[[m]](aligned$model, aligned$obs)
  })
  data.frame(metric = toupper(metrics), value = unlist(results, use.names = FALSE))
}

##' Plot model vs observations time series
##'
##' @param aligned data.frame with columns: time, model, obs
##' @return ggplot object
##' @importFrom rlang .data
plot_time_series <- function(aligned) {
  ggplot2::ggplot(aligned, ggplot2::aes(x = .data$time)) +
    ggplot2::geom_line(ggplot2::aes(y = .data$model, color = "Model")) +
    ggplot2::geom_line(ggplot2::aes(y = .data$obs, color = "Obs")) +
    ggplot2::labs(color = "", y = "value", title = "Model vs Observations") +
    ggplot2::theme_bw()
}
