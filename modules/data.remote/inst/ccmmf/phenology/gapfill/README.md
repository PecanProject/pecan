# Phenology date gap-fill (planting / harvest DOY)

After LandIQ–MSLSP matching, fill missing planting and harvest dates so
`no_mslsp` (and other) rows can still feed statewide harvest events.

**Rules (Dietze):**

1. If MSLSP date present → use it (`gapfill_*_source = mslsp`)
2. Else if `landiq_ADOY` present → `lm(doy ~ ADOY * CLASS)` (`lm_adoy`)
3. Else → mean DOY by crop CLASS (harvest: CLASS×PFT then CLASS) (`mean_crop`)

Planting outcome: `mslsp_OGI`. Harvest: `mslsp_OGMn` (row/rice) or `mslsp_OGD` (hay/woody).

## Run

```bash
export CCMMF_MANAGEMENT=/projectnb/dietzelab/ccmmf/management
export CCMMF_MATCHED_DIR=$CCMMF_MANAGEMENT/phenology/matched_landiq_mslsp_v4.1.2
module load R/4.4.0   # or whatever SCC image has working arrow

# 1) Fit on training years (default 2018–2023)
Rscript $CCMMF_MANAGEMENT/scripts/phenology/fit_phenology_gapfill_models.R

# 2) Apply overlay for each year
Rscript $CCMMF_MANAGEMENT/scripts/phenology/apply_phenology_gapfill.R 2016 2017 2018 2019 2020 2021 2022 2023
```

## Outputs

| Path | Contents |
|------|----------|
| `phenology/gapfill_models/phenology_date_gapfill_models.rds` | LMs + class means |
| `phenology/matched_landiq_mslsp_v4.1.2/gapfill_dates/assigned_year=Y_gapfilled.parquet` | Assigned + filled date columns |

Canonical `assigned_year=Y.parquet` is **not** overwritten.

## Events

`make_events_statewide.R` loads the gap-filled overlay when present. Harvest can
use filled dates for `no_mslsp` rows. Planting still needs MSLSP EVI for LAI
(rows without EVI are skipped). Phenology leaf-on/off still requires matched MSLSP.

Raw MSLSP extract output root: `phenology/raw_mslsp_v4.1.2/` (symlink
`raw_mslsp_v4.1` → `raw_mslsp_v4.1.2` for older paths).
