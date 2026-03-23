##' Run a simple benchmark pipeline
##'
##' Loads model output and observations, aligns by time,
##' computes RMSE and MAE, and returns a results table with a plot.
##'
##' @param model_path path to model output CSV file (must have 'time' and 'value' columns)
##' @param obs_path path to observations CSV file (must have 'time' and 'value' columns)
##' @param metrics character vector of metrics to compute. Options: "RMSE", "MAE"
##' @param tolerance_secs nearest-neighbor time tolerance in seconds (default 1 hour)
##'
##' @return list with: metrics (data.frame), aligned (data.frame), plot (ggplot)
##' @export
##'
##' @author Your Name
run_benchmark <- function(model_path, obs_path,
                          metrics = c("RMSE", "MAE"),
                          tolerance_secs = 3600) {

  # --- Load data ---
  model_df <- read.csv(model_path, stringsAsFactors = FALSE)
  obs_df   <- read.csv(obs_path,   stringsAsFactors = FALSE)

  # --- Ensure time column is POSIXct ---
  model_df$time <- as.POSIXct(model_df$time, tz = "UTC")
  obs_df$time   <- as.POSIXct(obs_df$time,   tz = "UTC")

  # --- Align by nearest time ---
  aligned <- align_by_time(model_df, obs_df, tolerance_secs = tolerance_secs)

  # --- Compute metrics ---
  results <- list()
  for (m in toupper(metrics)) {
    results[[m]] <- switch(m,
      "RMSE" = sqrt(mean((aligned$model - aligned$obs)^2, na.rm = TRUE)),
      "MAE"  = mean(abs(aligned$model - aligned$obs),     na.rm = TRUE),
      stop("Unknown metric: ", m)
    )
  }
  metrics_df <- data.frame(metric = names(results),
                           value  = unlist(results, use.names = FALSE))

  # --- Plot ---
  plot <- ggplot2::ggplot(aligned, ggplot2::aes(x = time)) +
    ggplot2::geom_line(ggplot2::aes(y = model, color = "model")) +
    ggplot2::geom_line(ggplot2::aes(y = obs,   color = "obs")) +
    ggplot2::labs(color = "", y = "value", title = "Model vs Observations")

  list(metrics = metrics_df, aligned = aligned, plot = plot)
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
