# Tillage

The tillage product estimates the timing and magnitude of tillage events from optical signatures of soil disturbance. NDTI is extracted to parcels, then summarized in fallow windows between harvest/termination (OGMn) and the next effective planting date (OGI). Event files copy SIPNET columns from that metrics table. Event copy: [events/README.md](../events/README.md). Commands: [Session 2](../documentation/sessions/02-phenology.md).

`tillage_metrics()` lives in [events/R/tillage_metrics.R](../events/R/tillage_metrics.R). `apply_tillage.R` writes `assigned_year=Y_tillage.parquet` (NDTI diagnostics plus `tillage_eff_0to1`). `make_events_statewide.R tillage` copies SIPNET columns only. Column dictionary: [data/tillage_metrics_metadata.csv](data/tillage_metrics_metadata.csv).

A drop in NDTI during the fallow window is consistent with residue incorporation and bare soil exposure. Published row-crop thresholds (for example percent change > 70) are an initial decision rule; they have been tested mostly on midwestern row crops.

## NDTI extract

Compute the Normalized Difference Tillage Index (NDTI) from HLS shortwave infrared (SWIR) and extract it to LandIQ parcels. Column dictionary: [extract/data/ndti_year_metadata.csv](extract/data/ndti_year_metadata.csv). Shared parcel-tile map: [hls/README.md](../hls/README.md).

NDTI is computed from SWIR1 and SWIR2:

```text
NDTI = (SWIR1 - SWIR2) / (SWIR1 + SWIR2)
```

Landsat (HLSL30): B06 / B07 (~1610 nm / ~2190 nm). Sentinel-2 (HLSS30): B11 / B12. Lower NDTI indicates reduced surface residue / more exposed soil. Scenes are preprocessed with the Fmask quality band to remove cloud, shadow, and snow (bits 1, 3, 4). Temporal resolution is limited by clear-day overpass frequency.

Year-ag parcels from `$LANDIQ_GAPFILLED` against the one geometry map. 2017 (no LandIQ year) still filters 2017 ag `parcel_id`s from gap-filled crops.

A year-level extract (no month argument) writes months overlapping Jan 1 Y through Dec 31 Y plus `HLS_DOWNLOAD_BUFFER_DAYS` (default 185) into hive `year=Y+1`. Single-month reruns do not add that shoulder. `NDTI_INCLUDE_LOOKAHEAD=0` or `NDTI_INCLUDE_LOOKBACK=0` restores calendar-year-only extract. Extract `$PRIOR_YEAR` then `$TARGET_YEAR` covers Jan of PRIOR through the forward shoulder of TARGET.

`DEMO_TILE` / `--tile` / `TILEWISE_ONE_TILE` limit extract and prep. Combine stacks every `tile=*.csv.gz` for that month. If the monthly parquet exists, combine skips unless overwrite is set.

Imagery layout: `$HLS_IMAGERY_ROOT/<tile>/images/<scene>/` with B06/B07 or B11/B12 plus Fmask.

Same tilewise pipeline as MSLSP extract:

1. **Prep.** Load `parcel_tiles.csv` filtered to year-ag IDs.
2. **Extract.** Per tile and month, read HLS scenes, apply Fmask, compute NDTI, and aggregate to the polygon with area-weighted mean and SD. `n_eff = w_valid^2 / sum_w2`; `na_frac` is masked fraction. When L30 and S30 share a day, keep the scene with more valid pixels at tillage-metrics time (extract keeps both until combine). Write `tilepieces_year=Y_month=MM/tile=TILE.csv.gz`.
3. **Combine.** Bind tilepieces, aggregate parcels that span tiles, write `ndti_year=Y_month=MM.parquet`.

One row per `parcel_id x scene date`. Hive: `$PRODUCTS_INVENTORY/tillage/ndti_v4.1.2/year=Y/`.

Orchestrator: `run_ndti.sh [options] YEARS`.

| Flag / env | Role |
|------------|------|
| `--months M` | Month list/range (default: 12 plus forward shoulder) |
| `--jobs N` | Concurrent month extract processes (`NDTI_MONTH_JOBS`; scheduler default is job CPUs) |
| `--tile TILE` | One MGRS tile |
| `--prep-only` | Static prep cache only |
| `--no-extract` / `--no-combine` | Skip that step |
| `--overwrite` | Replace existing tilepieces / monthly parquet |
| `NDTI_PARCEL_YEARS` | Years whose ag IDs define extract parcels (Session 2: `$PRIOR_YEAR,$TARGET_YEAR`) |
| `HLS_DOWNLOAD_BUFFER_DAYS` | Forward shoulder after Dec 31 (default 185) |
| `NDTI_INCLUDE_LOOKAHEAD` / `NDTI_INCLUDE_LOOKBACK` | Set `0` to drop the shoulder |
| `NDTI_TERRA_THREADS` | terra threads (default 8; set to 1 when `--jobs` > 1) |
| `HLS_IMAGERY_ROOT` | Converted HLS scenes |
| `HLS_PARCEL_TILES_DIR` | Directory of `parcel_tiles.csv` |

Atomic: `extract_tiles.R YEAR [tile_id|month] [overwrite]`; `combine_year.R YEAR [tile_id|month] [overwrite]`.

| Path | Contents |
|------|----------|
| `tillage/ndti_v4.1.2/year=Y/ndti_year=Y_month=MM.parquet` | Monthly output |
| `.../tilepieces_year=Y_month=MM/` | Per-tile intermediates + `_tile_timing.csv` |

## Tillage metrics

Fallow windows are the interval between OGMn (onset of greenness minimum at the end of the previous crop cycle) and OGI (onset of greenness in the following cycle). They can cross calendar years. Output `year` is the harvest / OGMn year, not the next OGI. PFT `other` (idle/fallow LandIQ) is skipped. Prefer `gapfill_dates/assigned_year=Y_gapfilled.parquet` when it is newer than the assigned table; otherwise use assigned. Keep `assigned_by` in `matched` / `no_mslsp` / `no_match` when gap-fill provenance exists; otherwise keep `"matched"` only. Rows need at least one of OGI or OGMn.

`apply_tillage.R Y` reads NDTI from Jan 1 (Y-1) through Dec 31 Y plus `HLS_DOWNLOAD_BUFFER_DAYS` (default 185), plus matched overlay years that overlap that window so the next OGI can close a fallow. It writes harvest-year Y and amends Y-1 so a Y-1 harvest to Y planting is not dropped when you run TARGET. `$TARGET_YEAR` harvest rows stay partial until the next update. Dedup is `parcel_id` + `OGMn_date` (keep first after sort).

1. Join NDTI scenes to phenology dates (`OGI_date`, `OGMn_date`) per parcel.
2. Smooth NDTI once per parcel-day: keep the scene with more valid pixels when L30 and S30 share a day; fill gaps with linear interpolation (`zoo::na.approx`); four-day symmetric moving average.
3. Build fallow periods: `fallow_start = OGMn`, `fallow_end = lead(OGI)`. Drop windows missing either date.
4. In each window, take the date of minimum smoothed NDTI (most negative Delta-NDTI); record the pre-minimum peak on or before that date; `ndti_pct_change = (max_pre - min) / max_pre * 100`. Summaries include dates of max/min, valid pixel counts, and uncertainties.
5. If the min day has `n_valid == 0`, pool SD from neighboring valid scenes (days that were interpolated). Map fractional drop `ndti_pct_change / 100` to `tillage_eff_0to1` via `PEcAn.data.land::ndti_to_sipnet_tillage()`.

Default mapper: `tillage_eff = clamp((delta - 0.30) * 2.5, 0, 1)`. Values at or below 0.30 are treated as no-till (0); the ramp reaches 1 at 0.70. Negative delta is treated as no-till. NA delta stays NA.

| Name | Default | Role |
|------|---------|------|
| `MATCHED_DIR` | matched hive | Assigned / gap-filled overlay |
| `HLS_DOWNLOAD_BUFFER_DAYS` | `185` | Forward end of the NDTI window after Dec 31 Y |
| `TILLAGE_PARCEL_CHUNK` | `3000` | Parcels per `apply_tillage.R` chunk |
| `GAPFILL_DATES_DIR` | `$MATCHED_DIR/gapfill_dates` | Gap-filled overlay |

Script: `apply_tillage.R YEAR`. Output: `$PRODUCTS_INVENTORY/tillage/tillage_metrics/assigned_year=Y_tillage.parquet`.
