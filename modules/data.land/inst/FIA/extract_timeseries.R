## extract_timeseries.R
## Run once on SCC to extract all model + obs timeseries from SDA_runs_vec.
## Saves timeseries_data.Rdata to the same directory as this script.
## That file is then loaded instantly by uncertainty_plots_new.Rmd.

library(dplyr)
library(tidyr)
library(purrr)
library(PEcAn.settings)

# ============================================================
# CONFIG
# ============================================================
runs     <- c("base", "+GWBI", "+LT", "+LT_GWBI", "+FIA", "+FIA_GWBI", "+LT_FIA", "+LT_FIA_GWBI")
base_dir <- "/projectnb/dietzelab/cwebb16/SDA_runs"
pecan_xml <- "/projectnb/dietzelab/cwebb16/SDA/pecan.xml"
out_file <- "/projectnb/dietzelab/cwebb16/SDA/Validations/timeseries_data.Rdata"

state_vars <- c("AbvGrndWood", "LAI", "SoilMoistFrac", "TotSoilCarb", "GWBI")
n_state    <- length(state_vars)
years      <- 2012:2024    # timestep 1 = 2012, ..., 13 = 2024
n_years    <- length(years)

unit_conv <- c(
  AbvGrndWood   = 1,
  LAI           = 1,
  SoilMoistFrac = 1,
  TotSoilCarb   = 1,
  GWBI          = 1
)

# ============================================================
# 1. SITE INFO FROM PECAN.XML
# ============================================================
message("Loading site info from pecan.xml...")
settings <- PEcAn.settings::read.settings(pecan_xml)
site_info_df <- settings$run %>%
  map("site") %>%
  map_dfr(function(s) data.frame(
    site_idx  = as.integer(s$id),
    lat       = as.numeric(s$lat),
    lon       = as.numeric(s$lon),
    site_name = as.character(s$name),
    stringsAsFactors = FALSE
  ))

n_sites <- nrow(site_info_df)
message("  ", n_sites, " sites")

# ============================================================
# 2. HELPER: column indices for a variable in analysis matrix
# ============================================================
get_var_cols <- function(var, mat) {
  uniq   <- unique(colnames(mat))
  stride <- length(uniq)          # actual vars per site in THIS matrix
  v      <- which(uniq == var)
  if (!length(v)) return(integer(0))
  v + (0:(ncol(mat) / stride - 1)) * stride
}

# ============================================================
# 3. EXTRACT MODEL TIMESERIES FROM analysis.all AND forecast.all
#    One row per run x year x site x variable
# ============================================================

extract_sda_df <- function(run_list, base_dir, years, state_vars, n_sites, unit_conv, slot) {
  rows <- list()
  for (run in run_list) {
    message("  Run: ", run, " [", slot, "]")
    f <- file.path(base_dir, run, "sda.all.forecast.analysis.Rdata")
    e <- new.env(); load(f, envir = e)
    sda_list <- e[[slot]]; rm(e)

    for (t in seq_along(sda_list)) {
      mat <- as.matrix(sda_list[[t]])
      yr  <- years[t]
      for (var in state_vars) {
        cols <- get_var_cols(var, mat)
        if (!length(cols)) next
        ens_mat <- mat[, cols, drop = FALSE]
        for (s in seq_len(n_sites)) {
          ens <- ens_mat[, s]
          ens <- ens[is.finite(ens)]
          if (length(ens) < 5) next
          rows[[length(rows) + 1]] <- data.frame(
            run      = run,
            year     = yr,
            site_idx = s,
            variable = var,
            Mean     = mean(ens) * unit_conv[var],
            SD       = sd(ens)   * unit_conv[var],
            stringsAsFactors = FALSE
          )
        }
      }
    }
  }
  bind_rows(rows)
}

message("Extracting analysis timeseries...")
model_df <- extract_sda_df(runs, base_dir, years, state_vars, n_sites, unit_conv, "analysis.all")
message("  model_df rows: ", nrow(model_df))

message("Extracting forecast timeseries...")
forecast_df <- extract_sda_df(runs, base_dir, years, state_vars, n_sites, unit_conv, "forecast.all")
message("  forecast_df rows: ", nrow(forecast_df))

# ============================================================
# 4. ALIGN EACH RUN TO BASE START VALUE (per site x variable)
#    Applied to forecast_df only; analysis (model_df) left unaligned
#    so true starting-point differences from assimilation are visible.
# ============================================================
model_df <- model_df %>% mutate(Mean_shifted = Mean, Offset = 0, RunStart = Mean, BaseStart = Mean)

fc_base_starts <- forecast_df %>%
  filter(run == "base", year == min(years)) %>%
  select(site_idx, variable, BaseStart = Mean)

forecast_df <- forecast_df %>%
  left_join(fc_base_starts, by = c("site_idx", "variable")) %>%
  group_by(run, site_idx, variable) %>%
  mutate(
    RunStart     = Mean[year == min(years)],
    Offset       = RunStart - BaseStart,
    Mean_shifted = Mean - Offset
  ) %>%
  ungroup()

# ============================================================
# 5. EXTRACT OBSERVATIONS FROM sda.outputN.Rdata (base run only)
# ============================================================
message("Extracting observations from sda.outputN.Rdata (base run)...")

obs_rows <- list()

for (t in seq_along(years)) {
  yr   <- years[t]
  path <- file.path(base_dir, "base", paste0("sda.output", t, ".Rdata"))
  if (!file.exists(path)) next
  e <- new.env(); load(path, envir = e)
  obs_mean_t <- e$sda.outputs$obs.mean; rm(e)

  for (s in seq_len(n_sites)) {
    df <- obs_mean_t[[as.character(s)]]
    if (is.null(df) || !is.data.frame(df)) next
    for (var in state_vars) {
      if (!(var %in% colnames(df))) next
      val <- df[[var]][1]
      if (!is.finite(val)) next
      obs_rows[[length(obs_rows) + 1]] <- data.frame(
        year     = yr,
        site_idx = s,
        variable = var,
        Obs      = val * unit_conv[var],
        stringsAsFactors = FALSE
      )
    }
  }
}

obs_df <- bind_rows(obs_rows)
message("  obs_df rows: ", nrow(obs_df))

# ============================================================
# 6. IDENTIFY FOCUS SITES
#    Largest / smallest AGB uncertainty change under +FIA
# ============================================================
message("Identifying focus sites...")

abg_cv <- model_df %>%
  filter(variable == "AbvGrndWood") %>%
  group_by(run, site_idx) %>%
  summarise(mean_sd = mean(SD, na.rm = TRUE), .groups = "drop")

abg_delta <- abg_cv %>%
  filter(run != "base") %>%
  left_join(abg_cv %>% filter(run == "base") %>%
              select(site_idx, base_sd = mean_sd),
            by = "site_idx") %>%
  mutate(delta_pct = 100 * (base_sd - mean_sd) / base_sd)

focus_fia <- abg_delta %>% filter(run == "+FIA")
site_max_reduction <- focus_fia %>% arrange(desc(delta_pct)) %>% slice(1) %>% pull(site_idx)
site_max_increase  <- focus_fia %>% arrange(delta_pct)        %>% slice(1) %>% pull(site_idx)

focus_sites <- unique(c(site_max_reduction, site_max_increase))
message("  Focus sites (max reduction, max increase): ", paste(focus_sites, collapse = ", "))

# ============================================================
# 7. FIA OBSERVATIONS from +FIA run (AbvGrndWood)
# ============================================================
message("Extracting FIA observations from +FIA run...")

fia_obs_rows <- list()

for (t in seq_along(years)) {
  yr   <- years[t]
  path <- file.path(base_dir, "+FIA", paste0("sda.output", t, ".Rdata"))
  if (!file.exists(path)) next
  e <- new.env(); load(path, envir = e)
  obs_t <- e$sda.outputs$obs.mean; rm(e)

  for (s in seq_len(n_sites)) {
    df <- obs_t[[as.character(s)]]
    if (is.null(df) || !is.data.frame(df)) next
    if (!("AbvGrndWood" %in% colnames(df))) next
    val <- df[["AbvGrndWood"]][1]
    if (!is.finite(val)) next
    fia_obs_rows[[length(fia_obs_rows) + 1]] <- data.frame(
      year     = yr,
      site_idx = s,
      FIA_obs  = val,
      stringsAsFactors = FALSE
    )
  }
}

fia_obs_df <- bind_rows(fia_obs_rows)
message("  FIA obs site-years: ", nrow(fia_obs_df), " (", length(unique(fia_obs_df$site_idx)), " sites)")

# ============================================================
# 8. FIA RESIDUAL ANALYSIS: base vs +FIA
# ============================================================
fia_residuals <- fia_obs_df %>%
  left_join(
    model_df %>%
      filter(variable == "AbvGrndWood", run %in% c("base", "+FIA")) %>%
      select(run, year, site_idx, Mean),
    by = c("year", "site_idx")
  ) %>%
  filter(!is.na(Mean)) %>%
  mutate(residual = Mean - FIA_obs)

fia_residual_site <- fia_residuals %>%
  group_by(run, site_idx) %>%
  summarise(
    mean_residual  = mean(residual,      na.rm = TRUE),
    mean_abs_resid = mean(abs(residual), na.rm = TRUE),
    n_years        = n(),
    .groups = "drop"
  )

fia_resid_delta <- fia_residual_site %>%
  pivot_wider(names_from = run,
              values_from = c(mean_residual, mean_abs_resid, n_years),
              names_glue = "{.value}_{run}") %>%
  rename_with(~ gsub("\\+", "plus_", .x)) %>%
  mutate(
    delta_residual     = `mean_residual_plus_FIA`  - mean_residual_base,
    delta_abs_residual = `mean_abs_resid_plus_FIA` - mean_abs_resid_base
  ) %>%
  left_join(site_info_df, by = "site_idx")

message("  Residual delta computed for ", nrow(fia_resid_delta), " sites")

# ============================================================
# 9. LT OBSERVATIONS from +LT run (AbvGrndWood)
# ============================================================
message("Extracting LT observations from +LT run...")

lt_obs_rows <- list()
for (t in seq_along(years)) {
  yr   <- years[t]
  path <- file.path(base_dir, "+LT", paste0("sda.output", t, ".Rdata"))
  if (!file.exists(path)) next
  e <- new.env(); load(path, envir = e)
  obs_t <- e$sda.outputs$obs.mean; rm(e)
  for (s in seq_len(n_sites)) {
    df <- obs_t[[as.character(s)]]
    if (is.null(df) || !is.data.frame(df) || !("AbvGrndWood" %in% colnames(df))) next
    val <- df[["AbvGrndWood"]][1]
    if (!is.finite(val)) next
    lt_obs_rows[[length(lt_obs_rows) + 1]] <- data.frame(
      year = yr, site_idx = s, LT_obs = val,
      stringsAsFactors = FALSE
    )
  }
}
lt_obs_df <- bind_rows(lt_obs_rows)
message("  LT obs site-years: ", nrow(lt_obs_df))

# ============================================================
# 10. GWBI OBSERVATIONS from +GWBI run
# ============================================================
message("Extracting GWBI observations from +GWBI run...")

gwbi_obs_rows <- list()
for (t in seq_along(years)) {
  yr   <- years[t]
  path <- file.path(base_dir, "+GWBI", paste0("sda.output", t, ".Rdata"))
  if (!file.exists(path)) next
  e <- new.env(); load(path, envir = e)
  obs_t <- e$sda.outputs$obs.mean; rm(e)
  for (s in seq_len(n_sites)) {
    df <- obs_t[[as.character(s)]]
    if (is.null(df) || !is.data.frame(df) || !("GWBI" %in% colnames(df))) next
    val <- df[["GWBI"]][1]
    if (!is.finite(val)) next
    gwbi_obs_rows[[length(gwbi_obs_rows) + 1]] <- data.frame(
      year = yr, site_idx = s, GWBI_obs = val,
      stringsAsFactors = FALSE
    )
  }
}
gwbi_obs_df <- bind_rows(gwbi_obs_rows)
message("  GWBI obs site-years: ", nrow(gwbi_obs_df))

# ============================================================
# 11. PER-SITE AGB UNCERTAINTY REDUCTION (LT vs FIA)
# ============================================================
message("Computing per-site uncertainty reduction...")

site_unc <- model_df %>%
  filter(variable == "AbvGrndWood", run %in% c("base", "+LT", "+FIA")) %>%
  mutate(cv_site = ifelse(abs(Mean) < 1e-6 | SD / abs(Mean) > 5,
                          NA_real_, SD / abs(Mean))) %>%
  group_by(run, site_idx) %>%
  summarise(mean_cv = mean(cv_site, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = run, values_from = mean_cv) %>%
  rename(cv_base = base, cv_lt = `+LT`, cv_fia = `+FIA`) %>%
  mutate(
    red_lt  = 100 * (cv_base - cv_lt)  / cv_base,
    red_fia = 100 * (cv_base - cv_fia) / cv_base,
    delta   = red_fia - red_lt
  ) %>%
  filter(is.finite(red_lt), is.finite(red_fia)) %>%
  left_join(site_info_df, by = "site_idx")

message("  site_unc rows: ", nrow(site_unc))

# ============================================================
# 12. GWBI SCATTER (base vs +GWBI: site-mean posterior vs obs)
# ============================================================
message("Computing GWBI scatter...")

gwbi_scatter <- gwbi_obs_df %>%
  left_join(
    model_df %>%
      filter(variable == "GWBI", run %in% c("base", "+GWBI")) %>%
      select(run, year, site_idx, post_mean = Mean),
    by = c("year", "site_idx")
  ) %>%
  filter(!is.na(post_mean)) %>%
  group_by(run, site_idx) %>%
  summarise(
    post_mean = mean(post_mean, na.rm = TRUE),
    obs       = mean(GWBI_obs,  na.rm = TRUE),
    .groups   = "drop"
  ) %>%
  mutate(
    residual = post_mean - obs,
    run      = factor(run, levels = c("base", "+GWBI"))
  )

message("  gwbi_scatter rows: ", nrow(gwbi_scatter))

# ============================================================
# SAVE
# ============================================================
# 13. LT-FIA COMPROMISE METRIC
#     For each site-year where both LT and FIA obs exist AND the +LT_FIA
#     analysis is available, compute where the analysis landed on the
#     spectrum between the two observations:
#       position = (analysis - FIA_obs) / (LT_obs - FIA_obs)
#     1 = stayed at LT, 0 = moved fully to FIA, 0.5 = equal split
# ============================================================
message("Computing LT-FIA compromise metric...")

ltfia_analysis <- model_df %>%
  filter(run == "+LT_FIA", variable == "AbvGrndWood") %>%
  select(year, site_idx, analysis = Mean)

compromise_df <- lt_obs_df %>%
  inner_join(fia_obs_df, by = c("year", "site_idx")) %>%
  inner_join(ltfia_analysis, by = c("year", "site_idx")) %>%
  mutate(
    obs_range = LT_obs - FIA_obs,
    position  = (analysis - FIA_obs) / obs_range
  ) %>%
  filter(abs(obs_range) > 1) %>%   # exclude site-years where LT and FIA barely differ
  left_join(site_info_df, by = "site_idx")

message("  compromise_df rows: ", nrow(compromise_df), " (", length(unique(compromise_df$site_idx)), " sites)")

# Per-site summary (mean position across years)
compromise_site <- compromise_df %>%
  group_by(site_idx) %>%
  summarise(
    mean_position  = mean(position,  na.rm = TRUE),
    mean_obs_range = mean(obs_range, na.rm = TRUE),
    n_years        = n(),
    .groups        = "drop"
  ) %>%
  left_join(site_info_df, by = "site_idx")

# ============================================================
message("Saving to ", out_file)
save(model_df, forecast_df, obs_df, site_info_df, focus_sites, abg_delta,
     fia_obs_df, lt_obs_df, fia_residuals, fia_residual_site, fia_resid_delta,
     gwbi_obs_df, site_unc, gwbi_scatter,
     compromise_df, compromise_site,
     runs, years, state_vars,
     file = out_file)
message("Done.")
