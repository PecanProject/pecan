# Climate-anchored prediction of crop planting and harvest timing

Can weather known in advance predict when California crops are planted and harvested,
and what does that imply under projected climate? Research code from the BU Ecological
Forecasting Lab (UROP).

## Idea

1. **Anchor.** Each season starts when it turns cold and wet. Two versions:
   `c7` (7-day mean minimum temperature below 7 C) and `rel` (below that season's own
   25th percentile), each with at least 3 wet days in the last 14, holding 5 days in a row.
2. **Daily score.** After the anchor, add up heat (mean temperature above 4 C) plus a dry-day flag
   (precipitation below 2.5 mm) plus a dry-soil flag (simple soil bucket).
3. **Prediction.** The day the running total reaches a threshold `Fcrit` is the predicted planting
   date (onset of greenness increase) or harvest date (onset of greenness minimum).
   `Fcrit` is calibrated per crop class x anchor x segment.
4. **Validation.** Leave-one-year-out (2019-2023) and forward in time (train on earlier years,
   test 2021-2023), both against a climatology baseline (mean training date).
5. **Projection.** Same model on 8 downscaled CMIP6 models (CalAdapt WRF 45 km, SSP3-7.0), seasons 2025-2051.
   Shifts are trends in days per decade (negative = earlier).

## Run order

Set the working directory to this folder, then run each script (batch jobs on the SCC).

| Script | Purpose | Main outputs (rds) |
|---|---|---|
| `00_setup.R` | settings, paths, shared functions (sourced by the others) | |
| `01_historical_model.R` | build, validate and calibrate on 2018-2023 | `calibrated_Fcrit`, `validation_*`, `validated_pairs`, `parcel_classes` |
| `02_future_projection.R` | project 2025-2051 for each climate model | `future_<model>`, `future_predictions_all_models`, `future_decomposition_all_models` |

Everything is written to `<base_dir>/rebuild_outputs` as `.rds` files. Paths and settings are at the top of `00_setup.R`
(inputs are described there). Environment variables `UROP_BASE`, `UROP_PHENO`, `UROP_FUTURE`,
`MODELS` and `NSLOTS` override paths, the list of climate models, and the core count.

## Validated classes

A class x anchor x segment pair is **validated** if it beats the baseline in at least 3 of 5
leave-one-year-out years and at least 2 of 3 forward years. **Strong** needs at least 4 of 5
leave-one-year-out years. `TIER` in `00_setup.R` chooses which list the projections use
(default `strong`).

## Notes

- 2018 has no anchors (the Sep-Feb search window needs 2017 data), so validation uses 2019-2023.
- The anchor thresholds (7 C, 2.5 mm, 3 of 14 days, 5 day hold) are conventional choices, not tuned.
- Parcels in the same 45 km cell and class share one future value, so averages are parcel weighted.
- Fcrit is calibrated on Daymet and applied to the coarser WRF output; level differences between
  the two are not corrected.
- Keep the order of the sections in `01_historical_model.R`: the calibration subsample uses
  `set.seed(42)`, and the order of random draws affects the fitted `Fcrit` values.
