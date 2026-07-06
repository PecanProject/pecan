# --- DUMMY LOGGER TO BYPASS DEPENDENCIES ---
PEcAn.logger <- new.env()
PEcAn.logger$logger.info <- function(...) cat("INFO:", ..., "\n")
PEcAn.logger$logger.warn <- function(...) cat("WARN:", ..., "\n")
PEcAn.logger$logger.severe <- function(...) stop("SEVERE:", ...)
`::` <- function(pkg, name) {
  pkg_name <- as.character(substitute(pkg))
  name_str <- as.character(substitute(name))
  if (pkg_name == "PEcAn.logger") return(get(name_str, envir = PEcAn.logger))
  
  # Safely call original ::
  orig_colon <- get("::", envir = asNamespace("base"))
  eval(substitute(orig_colon(pkg, name)))
}
# -------------------------------------------

# Source all R files in the benchmark module locally
lapply(list.files("modules/benchmark/R", pattern="\\.R$", full.names=TRUE), source)

# 1. LOAD YOUR CSV
csv_path <- "/home/ayushman1210/Downloads/observations.csv" # Replace with your actual path!

# Load the raw CSV
raw_df <- read.csv(csv_path, check.names = FALSE)

# Fix the unnamed 7th column (which holds the variable name)
names(raw_df)[7] <- "variable"

# 2. EXTRACT AND FORMAT THE OBSERVATIONS
obvs_df <- raw_df[raw_df$variable == "NEE", ]
obvs_df$time <- as.POSIXct(as.Date(obvs_df$max_date, format="%m/%d/%Y"))
obvs_df$obvs <- as.numeric(obvs_df$value)
obvs_df <- obvs_df[, c("time", "obvs")]

# 3. CREATE A FAKE "MODEL" DATASET (For testing purposes today)
set.seed(42)
model_df <- obvs_df
model_df$model <- model_df$obvs + rnorm(nrow(model_df), mean = 0, sd = 0.5)

# 4. ALIGN THE DATA
# Bypassing align_data to avoid lubridate requirement
aligned_df <- data.frame(
  time = obvs_df$time,
  model = model_df$model,
  obvs = obvs_df$obvs,
  model_q05 = model_df$model - runif(nrow(model_df), 0.5, 1.5),
  model_q95 = model_df$model + runif(nrow(model_df), 0.5, 1.5),
  obvs_sd = runif(nrow(obvs_df), 0.2, 0.8)
)

# 5. CALCULATE METRICS
metrics_list <- list(
  RMSE = metric_RMSE(aligned_df),
  R2   = metric_R2(aligned_df),
  MAE  = metric_MAE(aligned_df)
)

metrics_df <- data.frame(
  Metric = names(metrics_list),
  Value = as.numeric(metrics_list)
)

# 6. GENERATE PLOTS
p_timeseries <- metric_timeseries_plot(aligned_df, var = "NEE")
p_scatter <- metric_scatter_plot(aligned_df, var = "NEE")
p_residual <- metric_residual_plot(aligned_df, var = "NEE")

# 7. BUNDLE AND RENDER REPORT
benchmark_results <- list(
  metrics = metrics_df,
  aligned_data = aligned_df,
  plots = list(
    "NEE Timeseries" = p_timeseries,
    "NEE Scatter" = p_scatter,
    "NEE Residuals" = p_residual
  )
)

generate_validation_report(
  benchmark_results = benchmark_results,
  output_file = "AmeriFlux_Validation_Report.html",
  template = "modules/benchmark/inst/reports/Validation_report.qmd"
)
