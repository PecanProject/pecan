## extract_flux_cv.R
## Extracts flux CV from SIPNET NC output files.
## Saves flux_cv_data.Rdata separately (does NOT load timeseries_data.Rdata).

library(ncdf4)
library(dplyr)

base_dir  <- "/projectnb/dietzelab/cwebb16/SDA_runs"
out_file  <- "/projectnb/dietzelab/cwebb16/SDA/Validations/flux_cv_data.Rdata"
runs      <- c("base", "+GWBI", "+LT", "+LT_GWBI", "+FIA", "+FIA_GWBI", "+LT_FIA", "+LT_FIA_GWBI")
years     <- 2012:2024
flux_vars <- c("GPP", "AutoResp", "HeteroResp", "Transp")
n_sites   <- 247
n_ens_sample <- 20   # sample 20 of 100 ensemble members - enough for CV estimate

message("Starting flux CV extraction [", Sys.time(), "]")
message("Sampling ", n_ens_sample, " ensemble members per site-year")

all_rows <- list()

for (run in runs) {
  message("Run: ", run, " [", Sys.time(), "]")
  run_out <- file.path(base_dir, run, "out")

  # Collect all ensemble dirs and parse ens/site indices
  ens_dirs <- list.files(run_out, pattern = "^ENS-", full.names = TRUE)
  ens_meta <- data.frame(
    path = ens_dirs,
    ens  = as.integer(sub(".*ENS-(\\d+)-(\\d+)$", "\\1", basename(ens_dirs))),
    site = as.integer(sub(".*ENS-(\\d+)-(\\d+)$", "\\2", basename(ens_dirs))),
    stringsAsFactors = FALSE
  )

  # Sample ensemble members
  ens_ids   <- sort(unique(ens_meta$ens))
  ens_use   <- ens_ids[round(seq(1, length(ens_ids), length.out = min(n_ens_sample, length(ens_ids))))]
  ens_meta  <- ens_meta[ens_meta$ens %in% ens_use, ]

  # Accumulate: vals[[site]][[var]] = numeric vector across ens x year
  # Use flat list keyed by "site_year_var"
  accum <- list()

  for (i in seq_len(nrow(ens_meta))) {
    site <- ens_meta$site[i]
    path <- ens_meta$path[i]
    for (yr in years) {
      f <- file.path(path, paste0(yr, ".nc"))
      if (!file.exists(f)) next
      tryCatch({
        nc <- nc_open(f, suppress_dimvals = TRUE)
        for (var in flux_vars) {
          if (!(var %in% names(nc$var))) { nc_close(nc); next }
          val <- mean(ncvar_get(nc, var), na.rm = TRUE)
          key <- paste(site, yr, var, sep = "_")
          accum[[key]] <- c(accum[[key]], val)
        }
        nc_close(nc)
      }, error = function(e) NULL)
    }
    if (i %% 500 == 0) message("  processed ", i, "/", nrow(ens_meta), " dirs [", Sys.time(), "]")
  }

  # Compute CV from accumulator
  for (key in names(accum)) {
    parts <- strsplit(key, "_")[[1]]
    site  <- as.integer(parts[1])
    yr    <- as.integer(parts[2])
    var   <- parts[3]
    vals  <- accum[[key]][is.finite(accum[[key]])]
    if (length(vals) < 5) next
    m   <- mean(vals); s <- sd(vals)
    cv  <- if (m == 0 || !is.finite(m) || !is.finite(s) || s / abs(m) > 5) NA_real_ else s / abs(m)
    all_rows[[length(all_rows) + 1]] <- data.frame(
      run = run, variable = var, year = yr, site_idx = site, CV = cv,
      stringsAsFactors = FALSE
    )
  }
  message("  rows so far: ", length(all_rows))
}

flux_cv_df <- bind_rows(all_rows)
message("Total rows: ", nrow(flux_cv_df))

flux_cv_summary <- flux_cv_df %>%
  group_by(run, variable) %>%
  summarise(mean_cv = mean(CV, na.rm = TRUE), .groups = "drop")

flux_base_cv <- flux_cv_summary %>%
  filter(run == "base") %>%
  select(variable, base_cv = mean_cv)

flux_delta_cv <- flux_cv_summary %>%
  left_join(flux_base_cv, by = "variable") %>%
  mutate(pct_inc_cv = 100 * (mean_cv - base_cv) / base_cv)

message("Saving to ", out_file)
save(flux_cv_df, flux_delta_cv, file = out_file)
message("Done [", Sys.time(), "]")
