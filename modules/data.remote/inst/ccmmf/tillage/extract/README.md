# NDTI parcel extraction

Extract the **Normalized Difference Tillage Index (NDTI)** to the LandIQ parcel
level for California. NDTI is a shortwave-infrared (SWIR) index from Harmonized
Landsat Sentinel-2 (HLS) reflectance. Lower values indicate less crop residue /
more exposed soil, which is the remote signal used later to infer tillage in
fallow windows ([events/README.md](../../events/README.md)).

For each agricultural parcel the extract computes an **area-weighted NDTI** per
HLS scene from raw SWIR bands, masked with Fmask.

NDTI products are **monthly**: one output Parquet per (year, month), with one row
per parcel per HLS scene date.

- **HLSL (Landsat):** `(B06 - B07) / (B06 + B07)`
- **HLSS (Sentinel-2):** `(B11 - B12) / (B11 + B12)`
- Cloud / shadow / snow masked via Fmask (bits 1, 3, 4).

- **Input:** HLS reflectance + Fmask per scene, gap-filled LandIQ, parcel-tile map.
- **Output:** `$PRODUCTS_INVENTORY/tillage/ndti_v4.1/year=Y/ndti_year=Y_month=MM.parquet`.

This component mirrors the [`landiq-gapfill/`](../../landiq-gapfill/README.md) and
[`phenology/extract/`](../../phenology/extract/README.md) layout: a bash orchestrator
(`run_ndti.sh`), atomic R scripts per step, and shared `R/`.

```mermaid
flowchart LR
  IMG["HLS imagery + Fmask"] --> E["extract_tiles.R"]
  LIQ["Gap-filled LandIQ"] --> E
  MAP["parcel-tile map"] --> E
  E --> TP["tilepieces_year=Y_month=MM/"]
  TP --> C["combine_month.R"]
  C --> O["ndti_year=Y_month=MM.parquet"]
  O --> X["events/... tillage"]
```

Parent index: [../README.md](../README.md).
Pipeline map: [tree README](../../README.md).
MSLSP sibling: [phenology/extract/README.md](../../phenology/extract/README.md).
Shared helpers / parcel-tile map: [hls/README.md](../../hls/README.md).
Downstream tillage events: [events/README.md](../../events/README.md).

---

## Component layout

```
tillage/extract/
+-- README.md
+-- data/
+-- scripts/
|   +-- prep_static.R         one-time / debug: static prep cache only
|   +-- extract_tiles.R       HLS scenes -> tilepieces (one month)
|   +-- combine_month.R       tilepieces -> monthly Parquet
|   +-- R/
|       +-- tilewise_ndti_implementation.R
|       +-- ndti_combine.R
|       +-- ndti_run.R
+-- (shared tilewise framework in hls/R/)
```

---

## Before you run

| Prerequisite | Source |
|--------------|--------|
| HLS reflectance + Fmask | [HLS_Phenology](https://github.com/mrinareddy/HLS_Phenology) -> `$HLS_IMAGERY_ROOT/` |
| Gap-filled LandIQ crops | `LANDIQ_GAPFILLED` -> [landiq-gapfill](../../landiq-gapfill/README.md) product |
| Parcel geometry | `LANDIQ_HARMONIZED` -> `$LANDIQ_HARMONIZED/parcels-consolidated.gpkg` |
| Parcel-tile map | `hls_parcel_tile_map_v4.1.csv` - build once; see [hls/README.md](../../hls/README.md) |

Default imagery layout (set by `setup_env.sh`):

`$HLS_IMAGERY_ROOT/<tile>/images/<scene>/`

with B06/B07 or B11/B12 + Fmask per scene. Point `HLS_IMAGERY_ROOT` at that tree.

---

## Run a year

### Step 1 - Environment

`source` your `setup_env.sh` (Session 0), or set:

```bash
export CCMMF_ROOT="${CCMMF_ROOT:-$HOME/ccmmf}"
export PRODUCTS_INVENTORY="${PRODUCTS_INVENTORY:-$CCMMF_ROOT/products/inventory}"
export TILLAGE_ROOT="${TILLAGE_ROOT:-$CCMMF_CODE/tillage}"
export LANDIQ_GAPFILLED="$CCMMF_ROOT/LandIQ/gapfilled"
export LANDIQ_HARMONIZED="${LANDIQ_HARMONIZED:-$CCMMF_ROOT/LandIQ/work/03-final}"

export HLS_IMAGERY_ROOT=$HLS_IMAGERY_ROOT
export HLS_PARCEL_TILEMAP=$PRODUCTS_INVENTORY/hls_parcel_tile_map_v4.1.csv
```

### Step 2 - Orchestrator (recommended)

Why: one command runs extract then combine for every month; outputs land under
`$PRODUCTS_INVENTORY/tillage/ndti_v4.1/`.

```bash
$TILLAGE_ROOT/run_ndti.sh 2024
$TILLAGE_ROOT/run_ndti.sh --overwrite 2023
$TILLAGE_ROOT/run_ndti.sh --months 1-6 2024
```

Use `--no-extract` or `--no-combine` for individual steps; `--prep-only` for the
static prep cache only.

### Step 3 - Atomic scripts (optional)

```bash
Rscript $TILLAGE_ROOT/extract/scripts/extract_tiles.R 2024 3
Rscript $TILLAGE_ROOT/extract/scripts/combine_month.R 2024 3
Rscript $TILLAGE_ROOT/extract/scripts/combine_month.R 2023 6 overwrite
```

### Step 4 - Parallel months (optional)

Run months in parallel (separate terminals or your site's batch system):

```bash
for m in $(seq 1 12); do
  $TILLAGE_ROOT/run_ndti.sh --months "$m" 2024 &
done
wait
```

### Step 5 - Verify

See [Verify the output](#verify-the-output). No automatic QC step yet.

---

## Requirements

R packages: `sf`, `terra`, `data.table`, `stringr`, `arrow`, `dplyr`, `exactextractr`.
`NDTI_TERRA_THREADS` controls terra threads (default 8). On sites that use environment
modules, the orchestrator may load GDAL/NetCDF/R via `HLS_MODULES`.

---

## Data model

- **One row per `parcel_id x scene date`.** Each monthly Parquet holds every HLS scene
  in that month.
- **Hive-partitioned dataset:** `open_dataset(".../tillage/ndti_v4.1")` under
  `$PRODUCTS_INVENTORY`.
- **Area-weighted** over unmasked pixels, aggregated across tiles for boundary parcels.
- **Quality:** `n_eff = w_valid^2 / sum_w2`; `na_frac` is masked fraction.

---

## Backfill all years

```bash
for y in 2016 2018 2019 2020 2021 2022 2024; do
  $TILLAGE_ROOT/run_ndti.sh "$y"
done
$TILLAGE_ROOT/run_ndti.sh --overwrite 2023
```

---

## Verify the output

```r
library(arrow); library(dplyr); library(lubridate)
ds <- open_dataset(file.path(Sys.getenv("PRODUCTS_INVENTORY"), "tillage/ndti_v4.1"))

ds |> mutate(month = month(date)) |> count(year, month) |>
  collect() |> arrange(year, month) |> print(n = 60)

ds |> filter(year == 2024, month(date) == 3) |>
  summarize(n = n(),
            ndti_med = median(ndti_mean, na.rm = TRUE),
            na_med   = median(na_frac, na.rm = TRUE)) |>
  collect()
```

Per-tile timing: `.../year=2024/tilepieces_year=2024_month=03/_tile_timing.csv`.

---

## Output schema

See [data/ndti_year_metadata.csv](data/ndti_year_metadata.csv) (column dictionary).

---

## Special cases

- **No-LandIQ year (2017).** Requires gap-filled product and parcel-tile map with
  `year=2017` rows. See
  [landiq-gapfill](../../landiq-gapfill/README.md#special-case-no-landiq-year-2017).
- **Fmask masking.** Cloud (bit 1), shadow (bit 3), snow (bit 4).
- **Smoke test:** `TILEWISE_ONE_TILE=10SDH $TILLAGE_ROOT/run_ndti.sh --months 3 2024`

---

## Reference

| Path (under `$PRODUCTS_INVENTORY`) | Contents |
|----------------------------------|----------|
| `tillage/ndti_v4.1/year=Y/ndti_year=Y_month=MM.parquet` | Final monthly output |
| `tillage/ndti_v4.1/year=Y/tilepieces_year=Y_month=MM/` | Per-tile intermediates + `_tile_timing.csv` |
| `tillage/ndti_v4.1/year=Y/ndti_prep_static_year=Y.rds` | Cached per-year prep |
| `tillage/ndti_v4.1/logs/` | R logs |

- Orchestrator: [`../run_ndti.sh`](../run_ndti.sh) (`--help`)
- Downstream: [events/README.md](../../events/README.md) (tillage events)
- Training: [documentation/sessions/02-phenology.md](../../documentation/sessions/02-phenology.md)
