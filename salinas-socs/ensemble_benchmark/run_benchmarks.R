# Benchmarking pipeline for Salinas SOCs ensemble data using decoupled PEcAn.benchmark toolkit

library(dplyr)
library(lubridate)
library(ggplot2)

# Paths
base_dir <- file.path(getwd(), "salinas-socs/ensemble_benchmark")
model_csv_path <- file.path(base_dir, "ensemble_output.csv")
obvs_csv_path <- file.path(base_dir, "observations_soc.csv")

# Source PEcAn.benchmark functions directly
benchmark_r_dir <- file.path(base_dir, "../../modules/benchmark/R")
benchmark_r_files <- list.files(benchmark_r_dir, pattern = "\\.R$", full.names = TRUE)
sapply(benchmark_r_files, source)

# 1. Read and filter model data (EFI long format)
cat("Reading model ensemble data...\n")
model_data <- read.csv(model_csv_path) %>%
  filter(variable == "TotSoilCarb") %>%
  mutate(time = as.POSIXct(datetime, tz = "UTC"))

# 2. Read observations
cat("Reading observations...\n")
obvs_data <- read.csv(obvs_csv_path) %>%
  filter(variable == "TotSoilCarb", in_model_window == "True") %>%
  mutate(time = as.POSIXct(date, tz = "UTC"))

# 3. Process site-level ensemble benchmarks
sites <- unique(obvs_data$site_id)
results_list <- list()
plots_list <- list()

for (site in sites) {
  cat(sprintf("\n========================================\nProcessing Site: %s\n", site))

  # Subset site model data
  m_site <- model_data %>% filter(site_id == site)
  o_site <- obvs_data %>% filter(site_id == site)

  if (nrow(m_site) == 0 || nrow(o_site) == 0) {
    cat(sprintf("Skipping site %s due to missing data.\n", site))
    next
  }

  # Construct ensemble matrix via base R reshape
  ens_df <- reshape(m_site[, c("time", "parameter", "prediction")], 
                    idvar = "time", timevar = "parameter", direction = "wide")
  
  unique_times <- ens_df$time
  ens_mat <- as.matrix(ens_df[, -1, drop = FALSE])

  # Summary model dataframe (mean and 90% quantiles)
  m_summary <- data.frame(
    time = unique_times,
    value = rowMeans(ens_mat, na.rm = TRUE),
    model_q05 = apply(ens_mat, 1, quantile, probs = 0.05, na.rm = TRUE),
    model_q95 = apply(ens_mat, 1, quantile, probs = 0.95, na.rm = TRUE),
    site = site
  )

  # Observation dataframe
  o_summary <- data.frame(
    time = o_site$time,
    value = o_site$obs_mean,
    obvs_sd = o_site$obs_sd,
    site = site
  )

  # Align model and observations by time (half-year tolerance for annual alignment)
  aligned <- align_by_time(m_summary, o_summary, tolerance_secs = 365 * 86400 / 2)
  
  if (nrow(aligned) == 0) {
    cat(sprintf("No overlapping time points for site %s after alignment.\n", site))
    next
  }

  # Match ensemble matrix indices to aligned time points
  aligned_time_indices <- match(aligned$time, unique_times)
  aligned_ens_mat <- ens_mat[aligned_time_indices, , drop = FALSE]
  attr(aligned, "ensemble_matrix") <- aligned_ens_mat

  # Compute statistical metrics via PEcAn metric registry
  site_metrics <- compute_metrics(aligned, metrics = c("RMSE", "MAE", "R2", "COVERAGE", "CRPS"))
  cat("Computed Metric Scorecard:\n")
  print(site_metrics)
  results_list[[site]] <- site_metrics

  # Generate spaghetti + ribbon visualization
  p <- metric_timeseries_plot(aligned, var = paste("Site:", site, "- TotSoilCarb Ensemble vs Observations"))
  
  plots_list[[paste("Site", site)]] <- p
  
  plot_filename <- file.path(base_dir, paste0("plot_", site, ".pdf"))
  ggsave(plot_filename, plot = p, width = 8, height = 5)
  cat(sprintf("Saved plot: %s\n", plot_filename))
}

if (length(results_list) > 0) {
  final_scorecard <- do.call(rbind, results_list)
  cat("\nFinal Salinas SOCs Benchmark Scorecard:\n")
  print(final_scorecard)
  
  # Assemble benchmark results list for Quarto scorecard HTML report
  benchmark_results <- list(
    metrics = final_scorecard,
    plots = plots_list
  )
  
  html_output <- file.path(base_dir, "Salinas_SOC_Validation_Report.html")
  template_path <- file.path(base_dir, "../../modules/benchmark/inst/reports/Validation_report.qmd")
  
  generate_validation_report(
    benchmark_results = benchmark_results,
    output_file = html_output,
    template = template_path
  )
  cat(sprintf("\nGenerated Quarto HTML Scorecard: %s\n", html_output))
}
