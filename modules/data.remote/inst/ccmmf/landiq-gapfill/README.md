# LandIQ gap-fill

Fills missing LandIQ **attributes** (season-2 crop identity and peak-greenness day) and records how each fill was chosen. It does not change polygons or `parcel_id` -- geometry is owned by [cadwr-landuse](https://github.com/ccmmf/cadwr-landuse). How to run a routine year pair: [Session 1](../documentation/sessions/01-landiq.md).

---

## Data model

One row per `parcel_id` x `year` x `season`. Geometry is joined later from `$LANDIQ_HARMONIZED/parcels-consolidated.gpkg`.


| Season    | Role                                                            |
| --------- | --------------------------------------------------------------- |
| **2**     | Inventory main crop (almost every agricultural parcel)          |
| 1 / 3 / 4 | Extra / cover; sparse. 2016 has seasons 1-3 only; 2018+ has 1-4 |


CDL is an annual map. Crop identity is filled for season 2 only. Within-year ADOY can be filled on any season that already has an agricultural CLASS.


| Field                             | Meaning                                                                                  |
| --------------------------------- | ---------------------------------------------------------------------------------------- |
| `CLASS` / `SUBCLASS`              | LandIQ crop codes. After merge, `SUBCLASS` is on the **2021 DWR remote-sensing legend**. |
| `ADOY`                            | Adjusted day of year of peak NDVI. Missing for fill = not numeric, NA, or **0**.         |
| `subclass_source` / `adoy_source` | How that row got its crop / ADOY (lowercase).                                            |
| `COVER`                           | Derived cover-crop flag, written after merge. Not a fill.                                |


Column dictionaries: [crops_all_years_metadata.csv](data/crops_all_years_metadata.csv), [cdl_fractions_metadata.csv](data/cdl_fractions_metadata.csv), [LandIQ_cropCode_lookup_table.csv](data/LandIQ_cropCode_lookup_table.csv).

---



## Two modes

**Within-year** (usual): LandIQ exists for that calendar year. CLASS is kept. Missing season-2 SUBCLASS and invalid ADOY are filled.

**Full-gap** (exception): years in `LANDIQ_GAPFILL_FULL_GAP_YEARS` (default **2017**) have no usable LandIQ. Season-2 CLASS is invented first, then SUBCLASS, then ADOY. Other seasons are empty. See [Full-gap](#full-gap) for only the differences.

---



## Lookups

Eight tables live under `outputs/`. A later inventory year **reads** them; it does not recount them. Rebuild only if fill errors that a table is missing, or after a method, legend, or training-year change.

The CDL x LandIQ map was counted on season-2 agricultural parcels that had both maps in **2016-2023 except 2017**. The ADOY means and parcel history were taken from years with a usable observed peak day: **2018-2023**. That window already covers the crop mix and typical greenness dates. Fold newer observed years in only if you want them in the training window.


| Table                      | File                                           | What it answers                                          |
| -------------------------- | ---------------------------------------------- | -------------------------------------------------------- |
| `P(CDL | CLASS)`           | `cdl_prob_by_class_*.parquet`                  | Given LandIQ CLASS, which CDL codes usually appear       |
| `P(CDL | CLASS::SUBCLASS)` | `cdl_prob_by_subclass_*.parquet`               | Same at subclass                                         |
| `P(SUBCLASS | CLASS)`      | `landiq_subclass_frequency_*.parquet`          | How often each subclass occurs inside a CLASS            |
| County CSS mean            | `adoy_mean_county_class_subclass_*.parquet`    | Mean observed ADOY by county x CLASS x SUBCLASS x season |
| County CLASS mean          | `adoy_mean_county_class_*.parquet`             | Mean observed ADOY by county x CLASS x season            |
| Statewide CSS mean         | `adoy_mean_statewide_class_subclass_*.parquet` | Mean observed ADOY by CLASS x SUBCLASS x season          |
| Statewide CLASS mean       | `adoy_mean_statewide_class_*.parquet`          | Mean observed ADOY by CLASS x season                     |
| Observed history           | `adoy_observed_history_*.parquet`              | Parcel-level observed ADOY                               |


`ADOY_REFERENCE_STAT` defaults to `mean` (that is why the files are `adoy_mean_*`). `median` is allowed if you rebuild.

Year-patch parquets written during a run stay gitignored. Commands: `gapfill.R cdl-landiq-probs` and `gapfill.R adoy-ref`.

When those tables are built, and whenever crop or ADOY reads an older LandIQ year, `SUBCLASS` is remapped to the 2021 legend on the fly so votes and training share one code space. Merge does the same for the gap-filled product (see [Merge](#merge--product)).

---



## Within-year crop

**Who is filled:** season-2 rows with an agricultural CLASS (`is_agricultural` in the crop-code lookup), missing subclass (`NA` / `""` / `**`), and CLASS not in `X`, `YP`.

**Not filled:**

- CLASS -- observed LandIQ CLASS is kept.
- A specific subclass that is already present -- `subclass_source = observed`.
- `X` / `YP` with a blank or `**` subclass -- left blank; after merge the label is `X/I/YP (no subclass)`. `I` with a blank subclass gets the same label (it is allowed into the cascade, but a leftover `**` is not treated as a failed fill).

**After the cascade:** vineyard (`V`) still `**` becomes wine grapes (`LANDIQ_VINEYARD_FALLBACK_SUBCLASS`, default `"2"`) and `subclass_source` is set to `observed`. That is a product convention, not an empirical fill.

Dominant CDL for the year is the code with the largest parcel fraction in that year's fraction parquet.

First hit wins (`assign_subclass`):

| # | `subclass_source` | Rule |
|---|-------------------|------|
| 1 | `plurality` | Same parcel + same CLASS in other season-2 years; vote. Default pool = entire panel except the fill year |
| 2 | `emission_cdl` | Argmax of (subclass frequency inside CLASS) x P(dominant CDL given CLASS::SUBCLASS) if score > 0. The label name is historical. |
| 3 | `prior_only` | Argmax of how often each subclass occurs inside that CLASS |
| 4 | `unfilled` | Stays `**` |

Plurality weight is `1 / (1 + abs(year_dist))` so nearer years count more, unless `LANDIQ_SUBCLASS_PLURALITY_WEIGHT=count` (every vote equal). Set `LANDIQ_SUBCLASS_PLURALITY_POOL=neighbors` to vote only from the nearest LandIQ years instead of the whole panel.

Per-year crop patches are written under `outputs/` and consumed by merge.

---



## Within-year ADOY

Runs after crop so a filled subclass can inform matching. Valid original ADOY stays `observed`.

**Exempt CLASS:** `X`, `I` -> ADOY NA, `adoy_source = not_applicable`. `YP` **is not exempt.**

**Who is filled:** any season with invalid ADOY on an agricultural, non-exempt CLASS (LandIQ plus the within-year subclass overlay). Empty season shells (no CLASS) are not in the fill panel.

First hit wins:


| #   | `adoy_source`              | Rule                                                                                                                              |
| --- | -------------------------- | --------------------------------------------------------------------------------------------------------------------------------- |
| 1   | `county_class_subclass`    | County x CLASS x SUBCLASS x season mean                                                                                           |
| 2   | `temporal_neighbor`        | Same parcel / season / CLASS / SUBCLASS within +/- `ADOY_TEMPORAL_MAX_YEAR_GAP` (default 3); aggregate with `ADOY_REFERENCE_STAT` |
| 3   | `county_class`             | County x CLASS x season mean                                                                                                      |
| 4   | `statewide_class_subclass` | Statewide CLASS x SUBCLASS x season mean                                                                                          |
| 5   | `statewide_class`          | Statewide CLASS x season mean                                                                                                     |
| 6   | `unfilled`                 | No match                                                                                                                          |
| 7   | `multiuse_season2`         | Post-pass: `MULTIUSE=M`, not season 2, still invalid; copy season-2 ADOY if the crop matches                                      |


---



## Merge / product

`gapfill.R merge YEARS` writes `$LANDIQ_GAPFILLED/crops_all_years.parq`.

1. Base table = existing gap-filled product if present, else `$LANDIQ_HARMONIZED/crops_all_years.parq`.
2. Overlay crop and ADOY patches for the requested years. Other years are carried unchanged.
3. Remap every `SUBCLASS` to the 2021 DWR RS legend (`harmonized_SUBCLASS` in the crop-code lookup; grouped codes split with [LandIQ_grouped_subclass_cdl_split.csv](data/LandIQ_grouped_subclass_cdl_split.csv)). `CLASS` letters do not change across vintages. `$LANDIQ_HARMONIZED` is **not** already guaranteed to be on that legend -- trust the gap-filled product for 2021 codes.
4. Vineyard `**` -> wine grapes; normalize `subclass_source` (`OBSERVED` / `vineyard_fallback` -> `observed`).
5. Keep consolidated `parcel_id`s only.

**Inactive seasons:** within-year rows without a crop stay as LandIQ wrote them (ADOY may have been filled if they had a CLASS). Full-gap padded seasons (1/3/4) have crop fields and provenance **NA** -- not a fill outcome.

Polygons stay under harmonized. Join `parcels-consolidated.gpkg` when you need geometry.

If DWR adds or changes codes, update the lookup CSV before gap-fill and before rebuilding the probability tables.

---



## COVER

Not a fill. `scripts/R/cover_crop_landiq.R` flags cover-crop candidates on the product table after merge. Default-on in `run_gapfill.sh`. There is no `gapfill.R cover`.

**Candidates:** F/{2,11,12,16}, G/{2,6}, P/{1,3,4,6}.

On a season with a CLASS: `COVER = TRUE` when the pair is a candidate **and** CLASS or SUBCLASS differs from the previous cropped season on the same parcel; else `FALSE`. The first cropped observation on a parcel cannot alternate, so it is `FALSE`. No CLASS: `COVER = NA`.

---



## Full-gap

Same command order as within-year. Differences only:


| Step | What changes                                                                                                                                                                                                                                                                                                                   |
| ---- | ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------ |
| Crop | Invent season-2 **CLASS**, then SUBCLASS on that CLASS. CLASS = MAP of CDL likelihood plus neighbor transition (county matrix under `data/county_transition_matrices/`, statewide CSV fallback). Both neighbors: `(p_fwd+p_bwd+p_cdl)/3`. One neighbor: average the available temporal message with `p_cdl`. Other seasons NA. |
| ADOY | Season `LANDIQ_ADOY_DEFAULT_SEASON` only (default 2).                                                                                                                                                                                                                                                                          |


Needs neighbor LandIQ years, CDL fractions for the gap year, the transition matrices, and the eight lookup tables.

---



## Assumptions

These are not restated from the sections above:

- The default plurality pool is the **full** season-2 panel. A parcel with a long, inconsistent history can vote from distant years.
- County and statewide mean peak day by crop are treated as usable stand-ins for a missing `ADOY`.
- The CDL x LandIQ map is treated as stationary across its training window.
- Full-gap CLASS invention is a different problem from within-year SUBCLASS fill. Do not read 2017 provenance shares as if they were an ordinary inventory year.

QC (`gapfill.R qc`) writes `outputs/qc_gapfill_report.md` and summary CSVs. "Gap-filled subclass" excludes `observed`, `X/I/YP (no subclass)`, legacy `YP (no subclass)`, and `vineyard_fallback`. ADOY gap-filled excludes `observed` / `not_applicable`. Prefer a high season-2 observed share on inventory years; there is no pass/fail threshold in code. Runtime labels are lowercase (the metadata CSV still says `OBSERVED` in places).

---



## Appendix



### Paths


| Variable                         | Role / default                             |
| -------------------------------- | ------------------------------------------ |
| `LANDIQ_GAPFILL_ROOT`            | This package                               |
| `LANDIQ_HARMONIZED`              | Harmonized in (`$CADWR_WORK_DIR/03-final`) |
| `LANDIQ_GAPFILLED`               | Gap-filled out                             |
| `CDL_DIR` / `CDL_OUT_DIR`        | Rasters; fractions (OUT defaults to DIR)   |
| `COUNTY_TRANSITION_MATRICES_DIR` | Full-gap county matrices                   |
| `EXTERNAL_TRANSITION_MATRIX_CSV` | Statewide transition fallback              |


Path defaults: [setup_env.sh](../documentation/setup_env.sh).

### Years and modes


| Variable                                 | Default / behavior                                               |
| ---------------------------------------- | ---------------------------------------------------------------- |
| `LANDIQ_GAPFILL_FULL_GAP_YEARS`          | `2017`                                                           |
| `LANDIQ_GAPFILL_AVAILABLE_YEARS`         | Parquet years minus full-gap years                               |
| `LANDIQ_GAPFILL_NEIGHBORING_YEARS`       | Full-mode neighbor override; else nearest before/after available |
| `LANDIQ_GAPFILL_RUN_YEARS`               | CLI year fallback                                                |
| `LANDIQ_GAPFILL_START_YEAR` / `END_YEAR` | Inclusive range if RUN_YEARS unset                               |
| `LANDIQ_ADOY_DEFAULT_SEASON`             | `2`                                                              |




### Training


| Variable                                                | Default / behavior                           |
| ------------------------------------------------------- | -------------------------------------------- |
| `CDL_LANDIQ_TRAINING_YEARS`                             | Explicit probability training years          |
| `CDL_LANDIQ_TRAINING_YEAR_MIN` / `MAX`                  | Range (both required)                        |
| `CDL_LANDIQ_TRAINING_EXCLUDE_YEARS`                     | `2017`                                       |
| `LANDIQ_SUBCLASS_PRIOR_YEARS`                           | Season-2 years minus full-gap                |
| `LANDIQ_ADOY_TRAINING_YEARS`                            | Available LandIQ years                       |
| `LANDIQ_ADOY_TRAINING_EXCLUDE_YEARS`                    | Optional exclusions                          |
| `ADOY_REFERENCE_STAT`                                   | `mean`                                       |
| `ADOY_TEMPORAL_MAX_YEAR_GAP`                            | `3`                                          |
| `GAPFILL_REBUILD_EMISSION` / `GAPFILL_REBUILD_ADOY_REF` | Silent rebuild if true (prefer explicit CLI) |




### Crop / CDL knobs


| Variable                            | Default / behavior                               |
| ----------------------------------- | ------------------------------------------------ |
| `LANDIQ_SUBCLASS_PLURALITY_POOL`    | `panel` (`neighbors` = nearest only)             |
| `LANDIQ_SUBCLASS_PLURALITY_WEIGHT`  | `inverse_distance` (`count` = equal votes)       |
| `LANDIQ_VINEYARD_FALLBACK_SUBCLASS` | `"2"` (wine grapes); source forced to `observed` |
| `CDL_LANDIQ_LOOKUP_WEIGHTING`       | `fraction` (vs `dominant`)                       |
| `CDL_CLASS_OBS`                     | `fraction` for full-gap CLASS CDL message        |
| `GAPFILL_TRANSITION_LEVEL`          | `county`                                         |




### CLI

Year lists: `2023`, `2023,2024`, `2023 2024`, or `2023-2024`. Help: `run_gapfill.sh -h` / `Rscript gapfill.R -h`.

If years are omitted on year-aware `gapfill.R` commands, they fall back to `LANDIQ_GAPFILL_RUN_YEARS` (then `GAPFILL_YEAR`). Rejected names: `emission`, `product`, `cover`, `ensure-tables`, `shared-tables`.


| Step                 | Default in `run_gapfill.sh`    | CLI                                                 |
| -------------------- | ------------------------------ | --------------------------------------------------- |
| CDL fractions ensure | always (skip years that exist) | see CDL scripts                                     |
| CDL x LandIQ probs   | **off**                        | `gapfill.R cdl-landiq-probs` / `--cdl-landiq-probs` |
| crop                 | **on**                         | `gapfill.R crop YEARS` / `--no-crop`                |
| ADOY ref             | **off**                        | `gapfill.R adoy-ref` / `--adoy-ref`                 |
| adoy                 | **on**                         | `gapfill.R adoy YEARS` / `--no-adoy`                |
| merge                | **on**                         | `gapfill.R merge YEARS` / `--no-merge`              |
| COVER                | **on**                         | `scripts/R/cover_crop_landiq.R` / `--no-cover`      |
| qc                   | **on**                         | `gapfill.R qc YEARS` / `--no-qc`                    |




### CDL scripts


| Script                                          | Role                                                                                                     |
| ----------------------------------------------- | -------------------------------------------------------------------------------------------------------- |
| `scripts/cdl/download_cdl_nass.R`               | NASS national 30 m zip, clipped to California -> `$CDL_DIR/cdl_YYYY.tif` (skip if exists; years >= 2008) |
| `scripts/cdl/extract_cdl_fractions_by_parcel.R` | Zonal fractions vs `parcels-consolidated.gpkg` -> `cdl_fractions_year=YYYY.parquet`                      |


`run_gapfill.sh` ensures fractions for requested years before crop.

### Rebuild lookups

Only after a method, legend, or training-year change:

```bash
$LANDIQ_GAPFILL_ROOT/run_gapfill.sh --cdl-landiq-probs --adoy-ref 2016-2023
```

Pin `CDL_LANDIQ_TRAINING_YEARS` to reproduce a specific window.

### Developer map

- Entry: `scripts/gapfill.R` -> `gapfill_main()` in `scripts/R/gapfill_cli.R`; library via `scripts/R/bootstrap.R`.
- Crop cascade: `gapfill_subclass.R`. Within-year vs full: `gapfill_run.R` / `gapfill_class.R`.
- Probability build/load: `gapfill_emission.R`, `gapfill_lookup_*.R`.
- Legend remap: `landiq_rs_harmonize.R`.
- ADOY: `gapfill_adoy.R`. Merge: `build_landiq_product.R`.
- Full-gap transitions: `county_transition.R` + `data/county_transition_matrices/`.
- When the method changes, update **this README**. Keep Session 1 as the operational pointer.

