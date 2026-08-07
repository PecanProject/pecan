# Phenology date gap-fill (planting / harvest DOY)

Required step after LandIQ-MSLSP matching ([match/README.md](../match/README.md)):
fill missing planting and harvest dates so `no_mslsp` (and other) rows can still
feed statewide planting/harvest events. Parent index: [phenology/README.md](../README.md).

**Rules (Dietze):**

1. If MSLSP date present -> use it (`gapfill_*_source = mslsp`)
2. Else if `landiq_ADOY` present -> `lm(doy ~ ADOY * CLASS)` (`lm_adoy`)
3. Else -> mean DOY by crop CLASS (harvest: CLASSxPFT then CLASS) (`mean_crop`)

Planting outcome: `mslsp_OGI`. Harvest: `mslsp_OGMn` (row/rice) or `mslsp_OGD` (hay/woody).

## Run

```bash
export MANAGEMENT="${MANAGEMENT:-$CCMMF_ROOT/management}"
export MATCHED_DIR=$MANAGEMENT/phenology/matched_landiq_mslsp_v4.1.2

# Fit + apply overlays (default years 2016-2023 if none given)
$CCMMF_CODE/phenology/run_phenology_date_gapfill.sh 2023 2024

# or step-by-step:
# Rscript $CCMMF_CODE/phenology/gapfill/fit_phenology_gapfill_models.R
# Rscript $CCMMF_CODE/phenology/gapfill/apply_phenology_gapfill.R 2023 2024
```

## Outputs

| Path | Contents |
|------|----------|
| `phenology/gapfill_models/phenology_date_gapfill_models.rds` | LMs + class means |
| `phenology/matched_landiq_mslsp_v4.1.2/gapfill_dates/assigned_year=Y_gapfilled.parquet` | Assigned + filled date columns |

Canonical `assigned_year=Y.parquet` is **not** overwritten.

## Events

`make_events_statewide.R` prefers the gap-filled overlay (required in production).
Harvest can use filled dates for `no_mslsp` rows. Planting still needs MSLSP EVI
for LAI (rows without EVI are skipped). Phenology leaf-on/off still requires
matched MSLSP.

Raw MSLSP extract output root: `phenology/raw_mslsp_v4.1.2/` (symlink
`raw_mslsp_v4.1` -> `raw_mslsp_v4.1.2` for older paths).
