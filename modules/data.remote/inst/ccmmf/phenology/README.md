# Phenology

Planting, harvest/termination, and leaf-on / leaf-off dates come from satellite-derived greenness in the HLS phenology product ([MSLSP](https://www.earthdata.nasa.gov/data/catalog/lpcloud-mslsp30na-011)). The pipeline has three steps: generate phenological metrics at 30 m from HLS EVI2, extract those metrics to LandIQ parcels, and match them to parcel-year-season rows so SIPNET can be prescribed a sequence of management events.

Tile NetCDF production is [../hls/README.md](../hls/README.md). Commands: [Session 2](../documentation/sessions/02-phenology.md).

```
phenology/
  run_mslsp.sh      # extract + combine orchestrator
  extract/          # NetCDF -> parcel parquet
  match/            # assigned_year=Y.parquet
  gapfill/          # apply overlay; outputs/ = 2018-2023 fill estimates
```

`PHENOLOGY_ROOT` defaults to `$CCMMF_CODE/phenology`. Tile vs statewide is `--tile` / `DEMO_TILE` / unset. LandIQ identifies up to four seasons per year; MSLSP stores the dominant two phenological cycles. Phenology leaf-on / leaf-off (`50PCGI` / `50PCGD`) is formatted in `make_events_statewide.R`, not an apply table.

## Parcel extract

Extract MSLSP phenology metrics to LandIQ parcels (the field management units used as the spatial grain of SIPNET). This does not re-run the tile algorithm. Column dictionary: [extract/data/mslsp_year_metadata.csv](extract/data/mslsp_year_metadata.csv).

MSLSP is read from per-tile NetCDFs. Parcel summaries are area-weighted so each pixel contributes in proportion to the fraction of parcel area it covers (not as equal-count pixels). That weighting matters both within a tile and across tiles when parcels are split among multiple tiles. Tile outputs are then combined into a statewide annual table (one parquet file per year).

Only agricultural parcels (`is_agricultural == TRUE` for the extract year, from `$LANDIQ_GAPFILLED`). Geometry comes from `parcels-consolidated.gpkg` and `$HLS_ROOT/parcel_tiles.csv`. Tiles with agricultural parcels but missing NetCDF still run and write an empty tilepiece. Not every HLS tile has agricultural land in California. `DEMO_TILE` / `--tile` / `TILEWISE_ONE_TILE` limit extract and prep; combine still stacks every `tile=*.csv.gz` under `tilepieces_year=Y/`. If `mslsp_year=Y.parquet` already exists, combine skips unless overwrite is set. Overwrite rebuilds from all current tilepieces, not from one demo tile.

`terra::rast()` on MSLSP NetCDF needs GDAL HDF5/netCDF plugins. `libgdal-core` alone is not enough.

1. **Prep.** Load `parcel_tiles.csv` filtered to year-ag IDs. Write `tiles_to_run.txt` (CA `tileids.txt` order intersect the CSV).
2. **Extract.** Per MGRS tile, crop the NetCDF to parcels on that tile and run `exactextractr::exact_extract` on cycle 1 then cycle 2. Cycle 1 is the dominant amplitude; cycle 2 is secondary. Metrics include OGI, peak of season, OGMn, additional EVI-based phenology metrics, and QA, for up to two cycles per year. Mean and standard deviation are stored per metric. `n_eff = w_valid^2 / sum_w2`; `na_frac` is the fraction of the parcel with no data. Write `tilepieces_year=Y/tile=TILE.csv.gz`.
3. **Combine.** Bind all tilepiece CSV.gz files, aggregate parcels that span tiles (area-weighted), write `$MSLSP_EXTRACT_ROOT/year=Y/mslsp_year=Y.parquet`.

NetCDF path: `$MSLSP_NETCDF_ROOT/<tile>/phenoMetrics/MSLSP_<tile>_<year>.nc`.

One row per `parcel_id x year x cycle`. Keys: `parcel_id`, `year`, `cycle`. Coverage: `n_valid`, `w_valid`, `na_frac`. Phenology and EVI: `*_mean` / `*_sd`. QA: `*_mode` / `*_mode_frac`.

| Layer | Meaning | Represents |
|-------|---------|------------|
| OGI | Onset Greenness Increase (15% greenness increase) | Planting |
| 50PCGI | 50 Percent Greenness Increase | Phenology leaf-on (hay, woody) |
| Peak | Date of cycle peak | Match (which cycle) |
| OGD | Onset Greenness Decrease (10% greenness decrease) | Harvest (hay, woody) |
| 50PCGD | 50 Percent Greenness Decrease | Phenology leaf-off (hay, woody) |
| OGMn | Onset Greenness Minimum (85% greenness decrease) | Harvest (row, rice); tillage fallow |

Orchestrator: `run_mslsp.sh [options] YEARS` (single year, comma list, or inclusive range).

| Flag / env | Role |
|------------|------|
| `--prep-only` | Prep cache and `tiles_to_run.txt` only |
| `--no-extract` | Combine existing tilepieces |
| `--no-combine` | Extract tilepieces only |
| `--tile TILE` | Extract one tile (implies `--no-combine`) |
| `--task-tile` | Array: `TASK_ID` is a 1-based line in `tiles_to_run.txt` |
| `--overwrite` | Replace existing tilepieces / year parquet |
| `DEMO_TILE` / `TILEWISE_ONE_TILE` | Same one-tile restriction as `--tile` |
| `MSLSP_NETCDF_ROOT` | Tile NetCDF tree |
| `MSLSP_EXTRACT_ROOT` | Hive for parquet + tilepieces (default `$HLS_ROOT/MSLSP/raw_mslsp_v4.1.2`) |
| `HLS_PARCEL_TILES_DIR` | Directory of `parcel_tiles.csv` |

Atomic: `extract_tiles.R YEAR [tile_id] [overwrite]`; `combine_year.R YEAR [tile_id] [overwrite]`. Passing a tile to combine still reads every `tile=*.csv.gz` on disk.

| Path | Contents |
|------|----------|
| `$MSLSP_EXTRACT_ROOT/year=Y/mslsp_year=Y.parquet` | Parcel extract |
| `.../tiles_to_run.txt` | Tiles with year-ag parcels |
| `.../tilepieces_year=Y/` | Per-tile CSV.gz + `_tile_timing.csv` |

## Match

Match extracted parcel-level MSLSP cycles to LandIQ parcel-year-season rows. After this step the workflow knows what was planted during each phenological cycle. Matched rows carry `mslsp_*` timing and EVI used for planting, harvest/termination, and phenology events.

Input: gap-filled LandIQ (`crops_all_years.parq`), combined MSLSP parquet for the year, crop-code lookup. Output: `$PRODUCTS_INVENTORY/phenology/matched_landiq_mslsp_v4.1.2/assigned_year=Y.parquet` (statewide). With `DEMO_TILE` set, files land under `tile=$DEMO_TILE/`. Column dictionary: [match/data/assigned_year_metadata.csv](match/data/assigned_year_metadata.csv).

Only agricultural parcels (`is_agricultural == TRUE` in the 2021 LandIQ legend lookup). Left join: every ag parcel-year in LandIQ is written. Parcel-years with no combined MSLSP row get `assigned_by == "no_mslsp"`. LandIQ ADOY is peak greenness timing (adjusted day of year), not emergence or senescence. QC labels (`qc_adoy_vs_cycle`, `qc_heterogeneity`, `match_outcome`) distinguish matched rows from missing ADOY, cycle-season mismatch, few pixels, and heterogeneity; they do not drop rows from events.

Without a gap-fill overlay, event builders keep `assigned_by == "matched"`. With `gapfill_dates/` present, planting/harvest intake can also include `"no_mslsp"` / `"no_match"` rows that received filled dates.

`DEMO_TILE` keeps year-ag parcels whose `tile_id` is in `parcel_tiles.csv` and writes under `tile=$DEMO_TILE/`. Unset for statewide. `ASSIGN_PARCEL_IDS_FILE` is an optional extra allowlist when `DEMO_TILE` is unset.

The key rule is that LandIQ seasonal timing (ADOY when available) must fall within the cycle interval bounded by OGI and OGMn, or be flagged when outside that window. When multiple cycles are possible, assignment is resolved with cycle/season checks:

1. **Non-woody primary:** LandIQ ADOY inside the MSLSP cycle window `[OGI, OGMn]`.
2. **Non-woody tie-break:** nearest `Peak` to `ADOY`, then prefer cycle 1 over cycle 2.
3. **Woody:** strongest remaining MSLSP cycle (cycle 1 first). LandIQ ADOY is not used. `qc_adoy_vs_cycle` is `woody_strongest_cycle`; field-year `match_outcome` is `matched_woody_strongest_cycle`. On a parcel-year with both mature woody (`D` / `C` / `V` / `T`) and young woody (`YP` or `SPECOND=Y`), the mature season is assigned first. YP-only parcels still take the strongest cycle.
4. **Season priority:** season 2 (main crop) first when `CLASS` is present; season 1 prioritized for `MULTIUSE` D/M; then seasons 3/4. Young woody is after mature woody. Higher `PCNT` breaks remaining ties.
5. **QC:** an unused extra MSLSP cycle (`2cycles_1season`) does not override `matched_adoy_validated` when the assigned season is in-window.

Successful assignment: `assigned_by == "matched"`.

One row per `parcel_id x year x season` (long format). Matched rows include `mslsp_*` date/DOY and EVI from the assigned cycle. Filename `year` is the LandIQ assignment year; phenology event dates can fall in an adjacent calendar year for cross-year cycles.

| Name | Role |
|------|------|
| `MATCHED_DIR` | Output hive (default `$PRODUCTS_INVENTORY/phenology/matched_landiq_mslsp_v4.1.2`) |
| `DEMO_TILE` | One MGRS tile; writes `tile=$DEMO_TILE/` |
| `ASSIGN_PARCEL_IDS_FILE` | Extra parcel allowlist when `DEMO_TILE` is unset |
| `LANDIQ_GAPFILLED` | Gap-filled crops parquet |
| `MSLSP_EXTRACT_ROOT` | Combined MSLSP parquet hive |
| `LANDIQ_CROPCODE_CSV` | CLASS / SUBCLASS / PFT / `is_agricultural` |

Script: `match_landiq_mslsp.R YEAR`.

## Date gap-fill

Fill missing MSLSP date and EVI metrics after matching so parcel-year-season rows without a satellite cycle can still receive planting and harvest/termination dates (and leaf-on / leaf-off when 50PCGI / 50PCGD are filled). Match already writes observed MSLSP dates onto `assigned_by == "matched"` rows. Gap-fill does not replace those. Idle/fallow (`other`) is not filled.

`assigned_year=Y.parquet` is not overwritten. Apply writes `gapfill_dates/assigned_year=Y_gapfilled.parquet`. Observed metrics stay (`gapfill_date_source = mslsp` when any date metric on the row was observed). Do not convert dates with calendar `yday()`: averaging yday across Jan 1 turns winter OGI into a June mean. Date DOY is relative to the assigned year (1 = Jan 1; negative = prior calendar year). A prediction more than ~2 years from Jan 1 is discarded and the mean fallback is used. Woody uses means only (no ADOY regression). LandIQ ADOY 0 is missing; negative winter peaks are kept. Column dictionary: [gapfill/data/assigned_year_gapfilled_metadata.csv](gapfill/data/assigned_year_gapfilled_metadata.csv). Other overlay columns are unchanged from match: [match/data/assigned_year_metadata.csv](match/data/assigned_year_metadata.csv).

`make_events_statewide.R` prefers the gap-filled overlay. Phenology leaf-on / leaf-off is hay and woody only and still requires matched or filled `mslsp_50PCGI` / `mslsp_50PCGD`. Planting events skip hay and woody. Harvest fractions are `apply_harvest.R`.

Fit writes CSV + JSON. Apply is arithmetic on those tables (no `lm` / `predict()`). The 2018-2023 estimates ship in [gapfill/outputs/](gapfill/outputs/). Re-run fit only when you want new coefficients (fit default output is `$PRODUCTS_INVENTORY/phenology/gapfill_models`; set `GAPFILL_MODEL_DIR` if apply should read that instead). Train years and per-metric n / r_squared are in `phenology_gapfill_meta.json`.

Applied independently to each metric:

1. If the MSLSP value is present, keep it.
2. Else if PFT is row/rice/hay, LandIQ ADOY is observed, and CLASS is in the LM table: `predicted = intercept[CLASS] + slope_adoy[CLASS] * landiq_ADOY` (`lm_adoy`).
3. Else CLASS x SUBCLASS x PFT mean, then CLASS x PFT, then CLASS, then global (`mean_crop`). Woody always uses this step.

That LM is `value ~ landiq_ADOY * landiq_CLASS` (one intercept and one ADOY slope per CLASS). Training uses `assigned_by == "matched"` crop PFTs from `GAPFILL_TRAIN_YEARS` (default 2018-2023); the LM is fit on row / rice / hay only.

| Kind | Columns filled |
|------|----------------|
| Dates | `mslsp_OGI`, `mslsp_50PCGI`, `mslsp_OGMx`, `mslsp_Peak`, `mslsp_OGD`, `mslsp_50PCGD`, `mslsp_OGMn` |
| EVI | `mslsp_EVImax`, `mslsp_EVIamp`, `mslsp_EVIarea` |

Planting uses `mslsp_OGI` (effective planting date). Harvest/termination uses `mslsp_OGMn` (row/rice) or `mslsp_OGD` (hay/woody).

| Name | Default | Role |
|------|---------|------|
| `MATCHED_DIR` | matched hive | Assigned overlay |
| `GAPFILL_MODEL_DIR` | apply: `$CCMMF_CODE/phenology/gapfill/outputs`; fit: `$PRODUCTS_INVENTORY/phenology/gapfill_models` | Fill estimates |
| `GAPFILL_DATES_DIR` | `$MATCHED_DIR/gapfill_dates` | Apply output |
| `GAPFILL_TRAIN_YEARS` | `2018,2019,2020,2021,2022,2023` | Fit years |

Scripts: `fit_phenology_gapfill_models.R` (retrain); `apply_phenology_gapfill.R YEAR [YEAR ...]`.

| Path under `GAPFILL_MODEL_DIR` | Contents |
|------|----------|
| `phenology_gapfill_meta.json` | Train years, version, per-metric n / r_squared |
| `phenology_gapfill_lm.csv` | Per metric x CLASS: `intercept`, `slope_adoy`, `n` |
| `phenology_gapfill_means_class_subclass.csv` | CLASS x SUBCLASS x PFT |
| `phenology_gapfill_means_class_pft.csv` | CLASS x PFT |
| `phenology_gapfill_means_class.csv` | CLASS |
| `phenology_gapfill_means_global.csv` | Global mean per metric |
