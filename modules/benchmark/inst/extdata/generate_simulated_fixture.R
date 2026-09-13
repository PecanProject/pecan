# Generate Precomputed Simulated Observation and Model Ensemble Fixtures
# Uses maintainer @dlebauer's statistical error model formulation:
# o_{v,t} = m_{v,t} + S_v * (\beta_v + \epsilon_{v,t}), where \epsilon_{v,t} ~ N(0, \tau_v^2)

set.seed(3)

# 1. Generate synthetic hourly timesteps over 30 days
start_time <- as.POSIXct("2026-06-01 00:00:00", tz = "UTC")
time_seq <- seq(from = start_time, by = "1 hour", length.out = 720) # 30 days * 24 hours

# 2. Simulate diurnal cycle for Net Ecosystem Exchange (NEE) in umol m-2 s-1
hours <- as.numeric(format(time_seq, "%H"))
base_cycle <- -12 * sin(pi * (hours - 6) / 12) * (hours >= 6 & hours <= 18) + 4 * (hours < 6 | hours > 18)

# 3. Create an ensemble of 10 model runs around the base diurnal cycle
n_ensemble <- 10
n_steps <- length(time_seq)
ensemble_matrix <- matrix(NA, nrow = n_steps, ncol = n_ensemble)

for (i in 1:n_ensemble) {
  # Add small site parameter variation to each ensemble member
  member_bias <- rnorm(1, mean = 0, sd = 0.8)
  member_noise <- rnorm(n_steps, mean = 0, sd = 1.2)
  ensemble_matrix[, i] <- base_cycle + member_bias + member_noise
}

# Compute ensemble statistics
ensemble_mean <- rowMeans(ensemble_matrix)
ensemble_sd <- apply(ensemble_matrix, 1, stats::sd)
model_q2_5 <- apply(ensemble_matrix, 1, stats::quantile, probs = 0.025)
model_q97_5 <- apply(ensemble_matrix, 1, stats::quantile, probs = 0.975)

# 4. Generate simulated observations using maintainer's exact error model
output_scale <- stats::sd(ensemble_mean)  # S_v
relative_bias <- 0.3                      # \beta_v = 30% of SD
relative_error <- 0.5                     # \tau_v = 50% of SD

simulated_observation <- ensemble_mean + output_scale * (
  relative_bias + stats::rnorm(length(ensemble_mean), mean = 0, sd = relative_error)
)

# Observation uncertainty (standard error)
obs_sd <- rep(0.8, n_steps)

# 5. Format DataFrames
model_df <- data.frame(
  time = format(time_seq, "%Y-%m-%d %H:%M:%S"),
  ensemble_mean = round(ensemble_mean, 4),
  ensemble_sd = round(ensemble_sd, 4),
  model_q2.5 = round(model_q2_5, 4),
  model_q97.5 = round(model_q97_5, 4)
)
for (i in 1:n_ensemble) {
  model_df[[paste0("model_", i)]] <- round(ensemble_matrix[, i], 4)
}

obs_df <- data.frame(
  time = format(time_seq, "%Y-%m-%d %H:%M:%S"),
  obvs = round(simulated_observation, 4),
  obvs_sd = round(obs_sd, 4),
  variable = "NEE",
  unit = "umol m-2 s-1"
)

# 6. Save to CSV text files in inst/extdata/
extdata_dir <- file.path("modules", "benchmark", "inst", "extdata")
if (!dir.exists(extdata_dir)) {
  dir.create(extdata_dir, recursive = TRUE)
}

write.csv(obs_df, file.path(extdata_dir, "simulated_observations.csv"), row.names = FALSE)
write.csv(model_df, file.path(extdata_dir, "simulated_model_ensemble.csv"), row.names = FALSE)

cat("Successfully generated simulated observation and model ensemble CSV fixtures in:", extdata_dir, "\n")
