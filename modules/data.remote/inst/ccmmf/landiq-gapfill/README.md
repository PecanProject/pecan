# LandIQ gap-fill

This pipeline fills in missing crop information on top of the harmonized LandIQ
parcel table and writes an updated **consolidated-geometry** product with per-row
provenance.

It fills two things:

- **Crop identity** — missing `SUBCLASS` for years that have LandIQ data, and the
  full `CLASS` **and** `SUBCLASS` for a year with no LandIQ data at all (e.g. 2017).
- **`ADOY`** — peak-greenness day-of-year.

All `SUBCLASS` values are mapped to the **Nov-2021 DWR RS legend**.

- **Input:** `$CCMMF_LANDIQ_V4/crops_all_years.parq` (harmonized LandIQ).
- **Output:** `$CCMMF_LANDIQ_GAPFILL_PRODUCT/crops_all_years.parq`, plus a
  `parcels-consolidated.gpkg` symlink to the source geometry.

```mermaid
flowchart LR
  H["Harmonize new year (upstream)"] --> S["Set env vars"]
  S --> R["run_gapfill.sh (CDL -> crop -> ADOY -> product -> QC)"]
  R --> O["Updated product"]
```

## Add a new year (standard workflow)

This is the common case: a new LandIQ year is available and you want it in the product.

**Run the new year together with the previous one** — e.g. `2023,2024`, not the whole
history. Gap-fill borrows crop labels from neighboring years. When 2023 was last
processed, 2024 did not exist yet, so re-running 2023 lets it also draw on 2024 and
improve. Routine runs use the shipped trained tables, so you only need CDL for the
years you are filling — see [Routine runs vs rebuilds](#routine-runs-vs-rebuilds).

Requirements on SCC: `module load R/4.4.3`. R packages: `arrow`, `dplyr`, `sf`,
`terra`, `data.table`, `exactextractr`, `readr`, and `CropScapeR` (CDL download only).

### Step 1 — Harmonize the new year (upstream)

Done **outside this folder.** Add the new year's rows to
`$CCMMF_LANDIQ_V4/crops_all_years.parq` (see [pipeline.md](../documentation/pipeline.md)).
Do not continue until the new year is in that file.

### Step 2 — Set environment variables

```bash
export CCMMF_ROOT=/projectnb/dietzelab/ccmmf

export LANDIQ_GAPFILL_ROOT=$CCMMF_ROOT/management/landiq-gapfill
export CCMMF_LANDIQ_V4=$CCMMF_ROOT/LandIQ-harmonized-v4.1
export CCMMF_LANDIQ_GAPFILL_PRODUCT=$CCMMF_ROOT/LandIQ-harmonized-v4.1.2

# Use the shipped 2016-2023 trained tables as-is (a routine run does not retrain).
export CDL_LANDIQ_TRAINING_YEAR_MIN=2016
export CDL_LANDIQ_TRAINING_YEAR_MAX=2023
```

### Step 3 — Run the gap-fill

```bash
module load R/4.4.3
$LANDIQ_GAPFILL_ROOT/run_gapfill.sh 2023,2024
```

One command does it all: downloads any missing CDL (skips years already present),
fills `SUBCLASS` and `ADOY` for 2023 and 2024 using the shipped trained tables,
writes the updated product to `$CCMMF_LANDIQ_GAPFILL_PRODUCT/crops_all_years.parq`,
and writes a QC summary to `outputs/qc_gapfill_report.md`.
A routine run updates only the years you pass and carries the rest over from the
existing product, so adding a year is cheap.

On a routine run, emission tables should **not** rebuild. If they do, confirm
`CDL_LANDIQ_TRAINING_YEAR_MIN` and `CDL_LANDIQ_TRAINING_YEAR_MAX` are set (Step 2).

### Step 4 — Submit on the cluster (recommended)

CDL extraction is the slow part (~40 min / year), and `arrow` only loads reliably on a
compute node. Run the whole thing as one batch job:

```bash
qsub -l buyin -l h_rt=8:00:00 -v 'GAPFILL_ARGS=2023,2024' $LANDIQ_GAPFILL_ROOT/sge/run_gapfill.sge
```

To run CDL extract in parallel for both years first, then gap-fill:

```bash
for y in 2023 2024; do
  qsub -l buyin -v YEAR=$y,CCMMF_LANDIQ_V4=$CCMMF_LANDIQ_V4 \
    $LANDIQ_GAPFILL_ROOT/sge/extract_cdl_fractions.sge
done
# after both finish:
qsub -l buyin -l h_rt=8:00:00 -v 'GAPFILL_ARGS=--no-cdl 2023,2024' \
  $LANDIQ_GAPFILL_ROOT/sge/run_gapfill.sge
```

### Step 5 — Review QC and logs

Open `outputs/qc_gapfill_report.md` for gap-fill counts per year. Check the run logs if
anything looks off. See [Verify the output](#verify-the-output).

## Data model: how to read the output

The product is a **long table**: one row per `parcel_id × year × season`. Getting the
shape right matters for any downstream use (MSLSP, phenology, SIPNET).

- **Geometry is fixed by `parcel_id`.** There is one polygon per `parcel_id`, the same
  across all years. A polygon's per-year identity lives in attributes (the
  `UniqueID_YYYY` columns of `parcels.gpkg`), not in separate geometries.
  - `parcels.gpkg` — **602,196** polygons in upstream LandIQ (full statewide delivery).
  - `parcels-consolidated.gpkg` — **423,780**-polygon subset used for raster
    extraction (CDL/HLS) and **this product**. The gap-filled
    `crops_all_years.parq` always includes **only** consolidated `parcel_id`s.
- **Four seasons per parcel-year; season 2 is the main one.**
  - **Season 2** is the primary annual crop — populated for essentially every parcel.
  - **Seasons 1 / 3 / 4** are additional croppings (double/triple crop) and are
    **mostly empty** (e.g. in 2020, season 1 ≈ 7%, season 3 ≈ 2%, season 4 ≈ <1%
    populated).
  - 2016 has only seasons **1–3** (DWR added a 4th season slot later).

## Routine runs vs rebuilds

The statistical tables that map CDL crop signatures to LandIQ crops were **trained once
on 2016–2023 and are shipped with this folder** under `outputs/` (the `cdl_prob_*`,
`cdl_landiq_subclass_lookup_*`, and `adoy_*` files), along with the crop-code lookups in
`data/`.

- **Use the shipped tables for a routine new-year update.** Pin them with
  `CDL_LANDIQ_TRAINING_YEAR_MIN=2016` and `CDL_LANDIQ_TRAINING_YEAR_MAX=2023`
  (Step 2 above). The run uses these as-is and needs CDL only for the years you fill,
  not the full CDL history. Each parcel's own crop history and the peak-greenness
  reference still update automatically from the LandIQ panel. Routine users do not need
  to think about rebuild flags.
- **Rebuild the trained tables only when** you change the gap-fill logic, change the
  lookup tables, or want to regenerate the full product from scratch. Rebuilding
  retrains from the entire CDL history (2016 onward) and is covered in
  [Rebuild all years from scratch](#rebuild-all-years-from-scratch).

## Rebuild all years from scratch

Use this to rebuild the **entire** product — after changing gap-fill logic or lookups,
or to regenerate a clean v4.1.2. It **retrains** the emission/ADOY tables and
**re-extracts** CDL, so it needs the full CDL history on disk.

**The buildable range is 2016–2023.** 2017 is handled automatically as the no-LandIQ
year (see [the 2017 special case](#special-case-no-landiq-year-2017)). **2024 cannot be
rebuilt here:** there is no 2024 LandIQ source and no 2024 CDL. Once 2024 is harmonized
upstream and its CDL is present, extend the range to `2016-2024`.

1. **Start from source, not the previous product.** The product builder uses an existing
   product as its base when one is present, rebuilding only the years you pass and
   carrying the rest over. For a clean rebuild, move the current product aside so the
   build starts from the source `$CCMMF_LANDIQ_V4`:

   ```bash
   mv $CCMMF_LANDIQ_GAPFILL_PRODUCT ${CCMMF_LANDIQ_GAPFILL_PRODUCT}.bak-$(date +%Y%m%d)
   ```

2. **Submit the full rebuild:**

   ```bash
   qsub -l buyin -l h_rt=24:00:00 \
     -v 'GAPFILL_ARGS=--rebuild-cdl --rebuild-emission --rebuild-adoy-ref 2016-2023' \
     $LANDIQ_GAPFILL_ROOT/sge/run_gapfill.sge
   ```

   `--rebuild-cdl` re-extracts CDL fractions (the CDL GeoTIFFs are cached, so they are
   not re-downloaded); `--rebuild-emission` and `--rebuild-adoy-ref` retrain the lookup
   and ADOY-reference tables from 2016–2023.

   **Alternative — parallel CDL extract.** CDL is ~40 min per year and runs
   sequentially inside `run_gapfill.sh`. For many years, submit one extract per year in
   parallel, wait for all to finish, then run gap-fill with `--no-cdl`:

   ```bash
   module load R/4.4.3
   export CCMMF_ROOT=/projectnb/dietzelab/ccmmf
   export LANDIQ_GAPFILL_ROOT=$CCMMF_ROOT/management/landiq-gapfill
   export CCMMF_LANDIQ_V4=$CCMMF_ROOT/LandIQ-harmonized-v4.1

   # Optional: download GeoTIFFs in parallel (skip if cdl_YYYY.tif already in $CDL_DIR)
   for y in 2016 2017 2018 2019 2020 2021 2022 2023; do
     qsub -l buyin -v "YEARS=$y" $LANDIQ_GAPFILL_ROOT/sge/download_cdl_nass.sge
   done

   # Extract fractions in parallel (~40 min per year)
   for y in 2016 2017 2018 2019 2020 2021 2022 2023; do
     qsub -l buyin -v YEAR=$y,CCMMF_LANDIQ_V4=$CCMMF_LANDIQ_V4 \
       $LANDIQ_GAPFILL_ROOT/sge/extract_cdl_fractions.sge
   done

   # After all extracts finish, run the rest of the pipeline (no CDL step)
   qsub -l buyin -l h_rt=24:00:00 \
     -v 'GAPFILL_ARGS=--no-cdl --rebuild-emission --rebuild-adoy-ref 2016-2023' \
     $LANDIQ_GAPFILL_ROOT/sge/run_gapfill.sge
   ```

   The same pattern works for a routine new-year run — e.g. submit extract for 2023 and
   2024 in parallel, then `GAPFILL_ARGS='--no-cdl 2023,2024'`. See
   [scripts/cdl/README.md](scripts/cdl/README.md) for CDL download/extract details.

3. **When it finishes, [verify the output](#verify-the-output).**

### Runtime (approximate)

Rough per-step wall-clock on SCC (`buyin`, 4 cores / 64 GB). Multiply per-year steps by
the number of years you pass; one-time steps run once when their rebuild flag is set.

| Step | What it does | Typical wall-clock |
|----|----|----|
| CDL extract | parcel CDL fractions (per year; skips if already on disk) | ~40 min / year |
| Emission tables | CDL→subclass lookup and probability tables (one-time; `--rebuild-emission`) | ~10 min |
| Crop gap-fill | CLASS/SUBCLASS (per year; within-year) | ~12 min / year |
| Crop gap-fill | CLASS/SUBCLASS (per year; full-gap, e.g. 2017) | ~15 min / year |
| ADOY reference | peak-greenness reference tables (one-time; `--rebuild-adoy-ref`) | ~20 sec |
| ADOY gap-fill | ADOY (per year) | ~1 min / year |
| Product build | assemble `crops_all_years.parq` (once at end) | ~2 min |
| QC summary | provenance counts per year → `outputs/qc_gapfill_report.md` | ~30 sec / year |

**Example — routine new-year run** (`2023,2024`, shipped tables): CDL download +
extract for both years (~40 min / year), then crop + ADOY + product (~30 min) — about
**~2 hours** total with CDL run sequentially, or **~70 min** with CDL in parallel (Step 4).

**Example — full 2016–2023 rebuild** (all steps above,
`--rebuild-cdl --rebuild-emission --rebuild-adoy-ref`): about **~7–8 hours** with CDL run
sequentially. With [parallel CDL extract](#rebuild-all-years-from-scratch), the CDL phase
is about **~40 min** (one submission per year in parallel); the remaining steps take about
**~1h 30m**.

## Verify the output

After every run, review the **QC report** (written automatically), then check the logs
if needed.

### QC report

`outputs/qc_gapfill_report.md` summarizes each year you gap-filled:

- Rows per season
- **How many season-2 rows were gap-filled** for crop identity (`subclass_source`) and
  ADOY (`adoy_source`), with counts and percentages
- Full provenance breakdown (observed vs `emission_cdl` vs reference-table fills, etc.)

Companion CSVs:

| File | Contents |
|------|----------|
| `qc_gapfill_summary.csv` | One row per year — gap-fill totals |
| `qc_gapfill_summary_provenance.csv` | `subclass_source` / `adoy_source` counts |
| `qc_gapfill_summary_subclass.csv` | Gap-filled season-2 rows by `CLASS`, `SUBCLASS`, and `subclass_source` |

The orchestrator log also prints a one-line summary per year, e.g.
`subclass gap-filled 120,875 (20.07%); ADOY gap-filled 398,253 (66.13%)`.

Skip QC with `--no-qc` if you are only re-running an intermediate step.

### Check the logs

Cluster runs write logs under `landiq-gapfill/sge_logs/`:

| Submission | Log files |
|----|----|
| `run_gapfill.sge` | `gapfill.*.out` (orchestrator steps) and `gapfill.*.err` (R detail) |
| `extract_cdl_fractions.sge` | `cdl_extract.*.out` / `.err` |
| `download_cdl_nass.sge` | `cdl_download.*.out` / `.err` |

**A successful gap-fill run should show:**

- The years you passed, with the correct mode per year (`within-year` or `full-year` for
  2017).
- CDL steps for each year (`CDL download year=` / `CDL extract year=`), or
  `CDL fractions exist ... skipping extract` when fractions are already on disk.
- On a **routine** run: `Emission tables cached (...); skipping build` — not
  `Building emission tables...` (see [Troubleshooting](#troubleshooting)).
- `Crop gap-fill year=` and `ADOY gap-fill year=` for each year you passed.
- `Building gap-filled product for years: ...`, then `QC summary for years: ...`, then
  `Done.` at the end of the `.out` file.
- In the `.err` file: `Wrote ... rows -> .../crops_all_years.parq` and a symlink message
  for `parcels-consolidated.gpkg`.

**Stop and investigate if you see:**

- `ERROR:` or the job ends without `Done.`
- `Building emission tables...` on a routine new-year run (training-year pins in Step 2
  may be missing).
- CDL errors, missing GeoTIFF messages, or extract stalling without chunk progress in
  `.err`.
- `undefined symbol: curl_multi_poll` — submit via `qsub` on a compute node instead of
  running interactively on the login node.

### Optional: spot-check in R

The QC report covers routine checks. For deeper inspection, run on a compute node (see
[Troubleshooting](#troubleshooting) if `arrow` fails to load):

```r
library(arrow); library(dplyr)
d <- open_dataset(file.path(Sys.getenv("CCMMF_LANDIQ_GAPFILL_PRODUCT"), "crops_all_years.parq"))

# 1. Years present, and rows per year x season
d |> count(year, season) |> collect() |> arrange(year, season) |> print(n = 40)

# 2. New year: season 2 should be populated for essentially every parcel
d |> filter(year == 2024, season == 2L) |> summarize(n = n(), n_class = sum(!is.na(CLASS))) |> collect()

# 3. 2017 (if present): four seasons, season 2 active, seasons 1/3/4 padded
d |> filter(year == 2017) |> count(season, !is.na(CLASS)) |> collect()

# 4. Provenance distribution (observed vs filled vs padded)
d |> filter(year == 2024) |> count(subclass_source) |> collect()
d |> filter(year == 2024) |> count(adoy_source) |> collect()
```

Confirm that:

- `$CCMMF_LANDIQ_GAPFILL_PRODUCT/crops_all_years.parq` exists and was updated at the end
  of the run.
- The **new year appears** with the expected season rows (four seasons for 2017+; three
  for 2016).
- **Season 2** has a non-`NA` `CLASS` for essentially every parcel in each observed year.
- For the new year, `subclass_source` / `adoy_source` are mostly `OBSERVED` on season 2,
  with some `emission_cdl` / reference-table fills where source LandIQ had gaps.
- **2017** (if in the product): four seasons; season 2 carries predicted crops; seasons
  1/3/4 are `NA` with `subclass_source = adoy_source = "absent"`.

Provenance value meanings: [Output schema & provenance](#output-schema--provenance).

## Output schema & provenance

Reference for downstream users. The product has the source LandIQ columns (`parcel_id`,
`year`, `season`, `CLASS`, `SUBCLASS`, `COUNTY`, `ADOY`, `ACRES`, `UniqueID`,
`centx`/`centy`, `MULTIUSE`, irrigation/region fields, …) plus two provenance columns
added by gap-fill. `SUBCLASS` is on the Nov-2021 DWR RS legend.

**`subclass_source`** — how `CLASS`/`SUBCLASS` was set:

| Value | Meaning |
|----|----|
| `OBSERVED` | from source LandIQ |
| `plurality` | full-gap CLASS prediction; subclass by plurality of CDL evidence |
| `emission_cdl` | subclass from the CDL→subclass emission table |
| `prior_only` | subclass from the parcel's historical crop prior |
| `vineyard_fallback` | vineyard (`CLASS = V`) with no subclass in source or gap-fill; set to wine grapes (`SUBCLASS = 2`) |
| `unfilled` | ag parcel, no confident subclass (stays `**`) |
| `absent` | padded inactive-season row (no crop) |

**`adoy_source`** — how `ADOY` was set:

| Value | Meaning |
|----|----|
| `OBSERVED` | from source LandIQ |
| `temporal` | borrowed from the same parcel in a neighbor year |
| `county_class_subclass` / `county_class` | county-level reference table |
| `statewide_class_subclass` / `statewide_class` | statewide reference fallback |
| `multiuse_season2` | copied from season 2 for a `MULTIUSE = M` parcel |
| `not_applicable` | CLASS is ADOY-exempt |
| `unfilled` | no reference matched |
| `absent` | padded inactive-season row |

## Troubleshooting

| Symptom | Cause / fix |
|----|----|
| `undefined symbol: curl_multi_poll` loading `arrow` | The login node has an older system library than `arrow` expects. Submit via `qsub` / `run_gapfill.sge` on a compute node instead. |
| Emission tables rebuild on a routine run | Re-export `CDL_LANDIQ_TRAINING_YEAR_MIN=2016` and `CDL_LANDIQ_TRAINING_YEAR_MAX=2023` (Step 2). |
| `Missing subclass assignment output …` | The crop step did not run for that year. Run crop before ADOY/product (the orchestrator does this in order). |
| 2017 build fails on a transition-matrix path | County matrices live at `/projectnb/dietzelab/ananyak/county_crop_matrices` (`*_crop_matrix.csv`). Set `EXTERNAL_TRANSITION_MATRIX_CSV` and `COUNTY_TRANSITION_MATRICES_DIR` if those defaults are wrong. Only full-gap (2017) builds use these. |
| CDL extract errors / missing GeoTIFF | Run the CDL download step first, or place `cdl_YYYY.tif` in `$CDL_DIR`. |

## Special case: no-LandIQ year (2017)

**This is rare — 2017 is the only such year so far,** and it needs no special command.
When LandIQ released no tabular data for a calendar year, gap-fill predicts the full crop
identity (CLASS **and** SUBCLASS) for that year instead of only filling gaps in observed
data.

These "full-gap" years are detected automatically — they are listed in
`LANDIQ_GAPFILL_FULL_GAP_YEARS` (default `2017`) — so you just include the year in a
normal run:

```bash
$LANDIQ_GAPFILL_ROOT/run_gapfill.sh 2017
```

The year is written into the product with the **same columns and the same long
(multi-season) shape as every other year**:

- Only **season 2** is predicted (crop identity from the CDL-based prediction, ADOY from
  the ADOY gap-fill). Attribute columns are carried from the nearest neighbor year.
- Each filled parcel is then **padded to the full season grid** (seasons 1–4), mirroring
  how observed years store inactive seasons: parcel-static attributes (`ACRES`,
  `UniqueID`, `COUNTY`, `centx`/`centy`, …) are retained, while crop-instance columns
  (`CLASS`, `SUBCLASS`, `ADOY`, …) are `NA`. These padded rows carry
  `subclass_source = adoy_source = "absent"`.
- Only parcels with a valid ag CLASS in **both** neighbor years (2016 and 2018) and
  nonzero CDL mass are filled — the rest of the consolidated parcel set has no neighbor
  signal and is not synthesized.

**Extra inputs** used only for full-gap years: county transition CSVs and
`data/state_transition_matrix.csv` (the Markov transitions used to predict CLASS). These
arrive via symlink and can be relocated with `EXTERNAL_TRANSITION_MATRIX_CSV` /
`COUNTY_TRANSITION_MATRICES_DIR` (see [Troubleshooting](#troubleshooting)).

## Reference

| Path | Contents |
|----|----|
| `cdl/` | `cdl_fractions_year=YYYY.parquet` (per-parcel CDL fractions) |
| `outputs/` | Shipped trained tables, per-year gap-fill outputs, `qc_gapfill_report.md` |
| `data/` | Crop-code lookups; transition-matrix symlinks (2017 only) |
| `sge/` | Cluster submission scripts (`run_gapfill.sge`, CDL extract/download) |
| `$CCMMF_LANDIQ_GAPFILL_PRODUCT/` | Final product |

- CDL download/extraction: [scripts/cdl/README.md](scripts/cdl/README.md)
- All orchestrator flags: `./run_gapfill.sh --help`
- Upstream harmonization: [pipeline.md](../documentation/pipeline.md)
- Downstream (phenology / MSLSP): consumes this product directly; tabular rows and
  `parcels-consolidated.gpkg` use the same consolidated parcel set. See
  `../scripts/phenology/README.md`.
