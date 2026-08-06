# LandIQ gap-fill

This component fills missing **crop identity** and **peak greenness** on the
harmonized LandIQ parcel table so every parcel has usable main-season crop
information for phenology matching and event generation.

LandIQ is California's statewide field-level crop mapping product. After
[cadwr-landuse](https://github.com/ccmmf/cadwr-landuse) harmonizes geometry and
crop attributes across years, some parcels still lack `CLASS` / `SUBCLASS`
and/or `ADOY` (adjusted day of year for peak greenness). This component fills
those gaps, writes an updated product, and records per-row provenance.

It does **not** change geometry or `parcel_id`. All `SUBCLASS` values use the
**November 2021 DWR RS legend** (Department of Water Resources remote-sensing
legend). A routine run rewrites only the years you pass and carries every other
year from the existing gap-filled product (or from the harmonized input on first
build).

**Observed vs filled:** every output row has `subclass_source` and `adoy_source`.
On the shipped v4.1.2 product (season 2): 2023 modelled subclass = 6.58%;
2023 gap-filled ADOY = 62.43%; 2016 gap-filled ADOY = 90.46%; 2017 crop
identity = 100% modelled. Full counts:
[documentation/sessions/01-landiq.md](../documentation/sessions/01-landiq.md#observed-vs-filled-be-explicit),
[outputs/qc_gapfill_report.md](outputs/qc_gapfill_report.md).

**Key features:**

- Season-2 crop fill from USDA Cropland Data Layer (CDL) parcel fractions plus
  LandIQ history / emission tables
- `ADOY` fill from same-parcel neighbors and county/statewide crop reference
  tables
- Automatic CDL download and fraction extract for years you request
- Full-gap years (default **2017**) get CLASS+SUBCLASS prediction and season
  padding
- Shipped lookup tables under `outputs/` for routine year-pair updates (no
  retrain)

Pipeline map: [documentation/pipeline.md](../documentation/pipeline.md).
Training walkthrough: [documentation/sessions/01-landiq.md](../documentation/sessions/01-landiq.md).

## Inputs and outputs

| | Path |
|--|------|
| Harmonized input | `$CCMMF_LANDIQ_V4/crops_all_years.parq` (must already include the new year) |
| Gap-filled product | `$CCMMF_LANDIQ_GAPFILL_PRODUCT/crops_all_years.parq` (+ geometry symlink) |
| This component | `$LANDIQ_GAPFILL_ROOT` (defaults to `$CCMMF_CODE/landiq-gapfill`) |
| CDL GeoTIFFs | `$CDL_DIR/cdl_YYYY.tif` |
| CDL parcel fractions | `$LANDIQ_GAPFILL_ROOT/cdl/cdl_fractions_year=YYYY.parquet` |

Column dictionaries: [data/crops_all_years_metadata.csv](data/crops_all_years_metadata.csv),
[data/cdl_fractions_metadata.csv](data/cdl_fractions_metadata.csv).

Upstream geometry: [ccmmf/cadwr-landuse](https://github.com/ccmmf/cadwr-landuse).

## Data model

Long table: one row per `parcel_id` x `year` x `season`. Geometry is fixed by
`parcel_id` (one polygon for all years). This product uses consolidated parcels
only (`parcels-consolidated.gpkg`).

**Four seasons; season 2 is the inventory main crop**

| Season | Role | Typical share with a crop (of parcels; ~2020 example) |
|--------|------|------------------------------------------------------|
| 2 | Primary annual crop | ~100% of ag parcels |
| 1 | Extra cropping (often cover / early) | ~7% |
| 3 | Extra cropping | ~2% |
| 4 | Extra cropping | <1% |

2016 has seasons 1-3 only (DWR added season 4 later).

**Why crop/ADOY gap-fill targets season 2 only:** USDA CDL is an **annual**
map, so the CDL-based crop fill is defined for the main season. Seasons 1/3/4
keep observed LandIQ when present and are padded as `absent` when empty.
Further modelling of those sparse seasons is future work; there is no solid
statewide validation target for them yet, and they are a small fraction of
fields.

**`COVER` cover-crop flag:** boolean on each row from
[`scripts/R/cover_crop_landiq.R`](scripts/R/cover_crop_landiq.R)
(`attach_cover_column()` in the product build). `COVER=TRUE` when
CLASS/SUBCLASS is a cover-crop candidate **and** the parcel alternates from
the previous non-absent season (first observation cannot alternate). Padded
`absent` seasons are `FALSE`. This is how cover crops are carried in the
inventory product today (not a separate multi-PFT event stack).

## Core workflow

Confirm the new year exists in the harmonized table, then run the year pair
(prior + new) so the prior year can use the new series as neighbor context:

```bash
source "$CCMMF_CODE/documentation/setup_env.sh"
# CCMMF_LANDIQ_V4 should still point at the harmonized (pre-gap-fill) product
$LANDIQ_GAPFILL_ROOT/run_gapfill.sh ${PRIOR_YEAR},${TARGET_YEAR}
export CCMMF_LANDIQ_V4=$CCMMF_LANDIQ_GAPFILL_PRODUCT
```

Earlier years supply context only; shipped tables under `outputs/` are used
as-is. CDL extract is the slow step (~40 min per year); use a compute node if
the login node cannot load R `arrow`.

When the log ends with `Done.`, review
`$LANDIQ_GAPFILL_ROOT/outputs/qc_gapfill_report.md`.

### Orchestrator steps

`run_gapfill.sh` runs, in order:

1. **CDL** - California GeoTIFF -> `$CDL_DIR/cdl_YYYY.tif`, then parcel code
   fractions -> `$LANDIQ_GAPFILL_ROOT/cdl/cdl_fractions_year=YYYY.parquet`.
2. **Crop identity** - fill missing season-2 `CLASS` / `SUBCLASS` from CDL +
   history (season 2 only; CDL is annual -- see [Data model](#data-model)).
3. **ADOY** - fill missing season-2 peak day from neighbors / reference tables.
4. **Product** - merge into `$CCMMF_LANDIQ_GAPFILL_PRODUCT`, attach `COVER`,
   carry other years unchanged.
5. **QC** - provenance tallies by year.

Flags: `--no-cdl` / `--no-crop` / `--no-adoy` / `--no-product` / `--rebuild-cdl` /
`--rebuild-emission` / `--rebuild-adoy-ref` (see `run_gapfill.sh -h`).

Approximate runtime: routine `2023,2024` ~1-2 h; full 2016-2023 rebuild ~7-8 h
(sequential CDL).

## CDL (USDA Cropland Data Layer)

Normally handled inside `run_gapfill.sh`. Standalone:

```bash
$LANDIQ_GAPFILL_ROOT/run_gapfill.sh --no-crop --no-adoy --no-product 2024
# or:
Rscript $LANDIQ_GAPFILL_ROOT/scripts/cdl/download_cdl_nass.R 2024
Rscript $LANDIQ_GAPFILL_ROOT/scripts/cdl/extract_cdl_fractions_by_parcel.R 2024
```

Code names: [data/cdl_nass_cropland_code_lookup.csv](data/cdl_nass_cropland_code_lookup.csv).

## Provenance

Two columns record how each row was set. Downstream matching and events should
prefer observed values and treat filled rows according to these codes.

**`subclass_source`** (how `CLASS` / `SUBCLASS` was set):

| Value | Meaning |
|-------|---------|
| `observed` | From source LandIQ (includes defaulting vineyard `V/**` -> `V/2` wine grapes) |
| `plurality` | Full-gap CLASS prediction; subclass by plurality of CDL evidence |
| `emission_cdl` | Subclass from the CDL-to-subclass emission table |
| `prior_only` | Subclass from the parcel's historical crop prior |
| `X/I/YP (no subclass)` | Idle/fallow/young-perennial classes that keep `SUBCLASS = **` by design |
| `unfilled` | Ag parcel, no confident subclass (stays `**`) |
| `absent` | Padded inactive-season row (no crop) |

**`adoy_source`** (how `ADOY` was set):

| Value | Meaning |
|-------|---------|
| `observed` | From source LandIQ |
| `temporal` | Borrowed from the same parcel in a neighbor year |
| `county_class_subclass` / `county_class` | County-level reference table |
| `statewide_class_subclass` / `statewide_class` | Statewide reference fallback |
| `multiuse_season2` | Copied from season 2 for a `MULTIUSE = M` parcel |
| `not_applicable` | CLASS is ADOY-exempt |
| `unfilled` | No reference matched |
| `absent` | Padded inactive-season row |

**`COVER`** (cover-crop season flag; see [Data model](#data-model)):

| Value | Meaning |
|-------|---------|
| `TRUE` | Cover-crop candidate CLASS/SUBCLASS and alternation from prior non-absent season |
| `FALSE` | Not a cover crop (including padded `absent` seasons) |

Built by [`scripts/R/cover_crop_landiq.R`](scripts/R/cover_crop_landiq.R) during
the product step.

## Special case: 2017 (no LandIQ year)

Detected via `LANDIQ_GAPFILL_FULL_GAP_YEARS` (default `2017`). Predicts season-2
CLASS+SUBCLASS; pads other seasons as `absent`. Needs neighbor years and
transition matrices under `data/`.

```bash
$LANDIQ_GAPFILL_ROOT/run_gapfill.sh 2017
```

## Rebuild from scratch

Only after changing gap-fill logic or lookups - **not** for a routine year-pair
update. Retrains emission/ADOY tables and re-extracts CDL.

```bash
mv $CCMMF_LANDIQ_GAPFILL_PRODUCT ${CCMMF_LANDIQ_GAPFILL_PRODUCT}.bak-$(date +%Y%m%d)

$LANDIQ_GAPFILL_ROOT/run_gapfill.sh \
  --rebuild-cdl --rebuild-emission --rebuild-adoy-ref 2016-2023
```

Routine runs should **not** rebuild emission tables. If they do, pin training
bounds (for example `CDL_LANDIQ_TRAINING_YEAR_MIN=2016` and
`CDL_LANDIQ_TRAINING_YEAR_MAX=2023`) and rely on the shipped `outputs/` tables.
`setup_env.sh` also sets `LANDIQ_GAPFILL_BOUND_MIN` / `LANDIQ_GAPFILL_BOUND_MAX`.

## Spot-check

```r
library(arrow); library(dplyr)
d <- open_dataset(file.path(Sys.getenv("CCMMF_LANDIQ_GAPFILL_PRODUCT"), "crops_all_years.parq"))
d |> count(year, season) |> collect() |> arrange(year, season)
d |> filter(year == 2024, season == 2L) |>
  summarize(n = n(), n_class = sum(!is.na(CLASS))) |> collect()
```
