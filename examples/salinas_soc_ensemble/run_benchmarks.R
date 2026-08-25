# Benchmarking pipeline for Salinas SOCs ensemble data using decoupled PEcAn.benchmark toolkit

library(dplyr)
library(ggplot2)

# Parse command line arguments (e.g., Rscript run_benchmarks.R <path_to_model_csv>)
args <- commandArgs(trailingOnly = TRUE)

# Determine base directory portably
base_dir <- file.path(getwd(), "examples/salinas_soc_ensemble")
if (!dir.exists(base_dir)) {
  base_dir <- getwd()
}

model_csv_path <- if (length(args) >= 1) args[1] else file.path(base_dir, "ensemble_output.csv")
obvs_csv_path <- file.path(base_dir, "observations_soc.csv")

if (!file.exists(model_csv_path)) {
  PEcAn.logger::logger.severe(sprintf("Model CSV not found at: '%s'. Please pass the path to ensemble_output.csv as an argument:\n  Rscript run_benchmarks.R <path_to_ensemble_output.csv>", model_csv_path))
}

if (!requireNamespace("PEcAn.benchmark", quietly = TRUE)) {
  devtools::load_all(file.path(base_dir, "../../modules/benchmark"))
} else {
  library(PEcAn.benchmark)
}

# 1. Read and filter model data (EFI long format)
PEcAn.logger::logger.info("Reading model ensemble data...")
model_data <- read.csv(model_csv_path) %>%
  filter(variable == "TotSoilCarb") %>%
  mutate(time = as.POSIXct(datetime, tz = "UTC"))

# 2. Read observations
PEcAn.logger::logger.info("Reading observations...")
obvs_data <- read.csv(obvs_csv_path) %>%
  filter(variable == "TotSoilCarb", tolower(as.character(in_model_window)) == "true") %>%
  mutate(time = as.POSIXct(date, tz = "UTC"))

if (nrow(obvs_data) == 0) {
  PEcAn.logger::logger.severe("No observations found after filtering in_model_window.")
}

# 3. Process site-level ensemble benchmarks
sites <- unique(obvs_data$site_id)
results_list <- list()
plots_list <- list()

for (site in sites) {
  PEcAn.logger::logger.info("========================================")
  PEcAn.logger::logger.info("Processing Site:", site)

  # Subset site model data
  m_site <- model_data %>% filter(site_id == site)
  o_site <- obvs_data %>% filter(site_id == site)

  if (nrow(m_site) == 0 || nrow(o_site) == 0) {
    PEcAn.logger::logger.warn("Skipping site", site, "due to missing data.")
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
    PEcAn.logger::logger.warn("No overlapping time points for site", site, "after alignment.")
    next
  }

  # Match ensemble matrix indices to aligned time points
  aligned_time_indices <- match(aligned$time, unique_times)
  aligned_ens_mat <- ens_mat[aligned_time_indices, , drop = FALSE]
  attr(aligned, "ensemble_matrix") <- aligned_ens_mat

  # Compute statistical metrics via PEcAn metric registry
  site_metrics <- compute_metrics(aligned, metrics = c("RMSE", "MAE", "R2", "COVERAGE", "CRPS"))
  PEcAn.logger::logger.info("Computed Metric Scorecard for site:", site)
  print(site_metrics)
  results_list[[site]] <- site_metrics

  # Generate spaghetti + ribbon visualization
  p <- metric_timeseries_plot(aligned, var = paste("Site:", site, "- TotSoilCarb Ensemble vs Observations"))
  
  plots_list[[paste("Site", site)]] <- p
  
  plot_filename <- file.path(base_dir, paste0("plot_", site, ".pdf"))
  ggsave(plot_filename, plot = p, width = 8, height = 5)
  PEcAn.logger::logger.info("Saved plot:", plot_filename)
}

if (length(results_list) > 0) {
  final_scorecard <- do.call(rbind, results_list)
  PEcAn.logger::logger.info("Final Salinas SOCs Benchmark Scorecard:")
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
  PEcAn.logger::logger.info("Generated Quarto HTML Scorecard:", html_output)
}
