# salinas socs ensemble -- benchmarking test data

the salinas organic cropping systems model ensemble, in EFI long format, for testing the phase-4 benchmarking pipeline against the white_salinas SOC observations.

## files
- **`ensemble_output.csv`** -- the model side. 67,200 rows: 50 ensemble members x 8 systems x 84 months (2005-2011) x {TotSoilCarb, AGB}.
- **`observations_soc.csv`** -- the obs side. per-system SOC stock, aggregated from the white_salinas block-level data (4 reps -> mean + sd + se), converted to kg C m-2. 72 rows (8 systems x 9 study years); 56 fall in the 2005-2011 model window (`in_model_window == True`).
- `extract_salinas_ensemble_output.R` / `build_salinas_soc_obs.py` -- how each csv was produced.

## the csv schema (EFI long)
`scenario, datetime, site_id, lat, lon, pft, parameter, variable, variable_type, prediction`
- `parameter` = ensemble member id (1-50)
- `variable` = `TotSoilCarb` (soil carbon, the SOC target) or `AGB`
- `prediction` = value in kg C m-2
- `datetime` = monthly, ISO8601 UTC
- `site_id` = socs_sys1 .. socs_sys8

this is the same schema the statewide `ensemble_output.csv` uses, so anything you build here carries over.

## how to consume it
1. filter to what you're validating: `variable == "TotSoilCarb"`.
2. reshape to a per-site members x times array with `efi_long_to_array()` (in ccmmf/downscaling `R/efi_long_to_arrays.R`) -- note that helper currently keys on a column named `ensemble`, but this csv (and the EFI standard) uses `parameter`, so rename or adjust.
3. take q05/q95 across members for the ribbon, the mean for RMSE/bias/R2.
4. align to the obs with `PEcAn.benchmark::align_data()` -- the model is **monthly**, the SOC obs are **annual** (treatment mean +/- SE), so align monthly -> annual, per system.
5. score: ensemble mean for the point metrics; the ensemble for coverage / CRPS (aggregate coverage across all systems x years).

## caveats
- **monthly vs annual:** aggregate the monthly model to annual before comparing to the obs.
- **units + depth:** `TotSoilCarb` is the model soil-C pool (kg C m-2). the white_salinas obs is a 0-30 cm SOC stock (Mg C ha-1, x0.1 -> kg/m2). close, but not the exact same depth integral -- compare per system and expect an offset.
- **wide spread:** this is a mid-calibration iteration (itr2), so the ensemble SOC range is wide and partly unphysical at the extremes. expected; fine for exercising the pipeline.
- **AGB** is ~0 for annual crops -- included for context, not a validation target here.

## obs (`observations_soc.csv`)
columns: `site_id, study_year, year, date, variable(=TotSoilCarb), obs_mean, obs_sd, obs_se, n, min_depth_cm(0), max_depth_cm(30), units(kg C m-2), in_model_window, dataset_id`.
- `site_id` matches the model csv (socs_sys1..8); join on `site_id` and `variable` + year.
- `obs_mean` +/- `obs_se` are the treatment mean and standard error over the n=4 replicate blocks (with n=4 the SE/tails are rough, so aggregate coverage across all systems x years rather than reading one point).
- filter `in_model_window == True` for the 2005-2011 overlap.
- compare each system to its OWN SOC (a cell-to-cell match). do NOT validate against a compost or cover-crop contrast -- that's an averaged effect, a different estimand.
