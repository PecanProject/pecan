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
  aligned <- do.call(rbind, lapply(seq_len(nrow(model_df)), function(i) {
    diffs <- abs(as.numeric(difftime(obs_df$time, model_df$time[i], units = "secs")))
    nearest <- which.min(diffs)
    if (diffs[nearest] <= tolerance_secs) {
      data.frame(time  = model_df$time[i],
                 model = model_df$value[i],
                 obs   = obs_df$value[nearest])
    } else {
      NULL
    }
  }))
  aligned
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
plot_time_series <- function(aligned) {
  ggplot2::ggplot(aligned, ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = model, color = "Model")) +
    ggplot2::geom_line(ggplot2::aes(y = obs,   color = "Obs")) +
    ggplot2::labs(color = "", y = "value", title = "Model vs Observations") +
    ggplot2::theme_bw()
}
