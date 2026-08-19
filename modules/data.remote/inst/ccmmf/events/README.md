# Statewide event files

Management event files prescribe the sequence of planting, harvest/termination, phenology, and tillage that SIPNET uses as inputs. Planting, harvest, and tillage copy SIPNET columns from apply tables. Phenology formats overlay `mslsp_50PCGI` / `mslsp_50PCGD` as `leafon` / `leafoff`. Source and diagnostic columns stay on the apply tables. Commands: [Session 2](../documentation/sessions/02-phenology.md).

Related Session 3 layers (fertilizer, organic amendments, irrigation) are separate statewide builders.

Downstream of `event_files/`: [SIPNET handoff](../documentation/sessions/sipnet-handoff.md).

## Assumptions

`event_type` is required and opt-in: `phenology` | `planting` | `harvest` | `tillage`. No default bundle. CLI:

```
Rscript make_events_statewide.R <prior_year> <target_year> <event_type>
Rscript make_events_statewide.R <year> <event_type>
```

`MATCHED_DIR` is the input overlay (and planting/harvest apply tables). Event files write to `$EVENT_OUTPUT_DIR` (default `$PRODUCTS_INVENTORY/event_files`). Parquet is the table format; JSON is PEcAn nested-by-site. Event files have SIPNET columns only. Source and diagnostic columns stay on the apply tables: planting [traits/data/planting_apply_metadata.csv](../traits/data/planting_apply_metadata.csv), harvest [traits/data/harvest_apply_metadata.csv](../traits/data/harvest_apply_metadata.csv), tillage [tillage/data/tillage_metrics_metadata.csv](../tillage/data/tillage_metrics_metadata.csv). Overlay dates: [phenology/match/data/assigned_year_metadata.csv](../phenology/match/data/assigned_year_metadata.csv) and [phenology/gapfill/data/assigned_year_gapfilled_metadata.csv](../phenology/gapfill/data/assigned_year_gapfilled_metadata.csv).

Overlay intake prefers `gapfill_dates/assigned_year=Y_gapfilled.parquet` when present (includes filled dates and `gapfill_date_source` for `no_mslsp` / `no_match`). Falls back to `assigned_year=Y.parquet`. With gap-fill provenance, candidates are `assigned_by` in `matched` / `no_mslsp` / `no_match`; without it, only `"matched"`. Rows missing CLASS, SUBCLASS, or PFT are dropped.

For perennials (hay, woody), phenology events are leaf-on (`50PCGI`) and leaf-off (`50PCGD`). Annuals (row, rice) do not get phenology events; they get planting and harvest. PFT `other` is skipped. Planting events use OGI as the effective planting date for annuals (SIPNET has no seed stage; pools are initialized at seedling size). Planting skips hay, woody, and PFT `other`. Harvest skips PFT `other` and young woody (`SPECOND=Y` or `CLASS=YP`). Hay and woody harvest is dated at OGD; orchard clearing is `PFT=woody` and `destructive=TRUE` dated at OGMn. `make_events` copies rem/lit columns as written. Idle/`other` harvest lookup returns NULL. `destructive` is ignored on annual PFTs.

## Event types

| Type | Date | Logic |
|------|------|-------|
| Phenology | `mslsp_50PCGI` / `mslsp_50PCGD` | Hay and woody only. One `leafon` and one `leafoff` row per overlay row with that date. Columns: `event_type`, `site_id`, `date`. Annuals (row, rice) are skipped. |
| Planting | `mslsp_OGI` (or gap-filled planting date) | Copy `$MATCHED_DIR/assigned_year=Y_planting.parquet` (from `apply_planting.R`). Row and rice only. Initial C and N pools; see [traits/README.md](../traits/README.md). |
| Harvest | row/rice -> `mslsp_OGMn`; hay/woody -> `mslsp_OGD`; woody `destructive` -> `mslsp_OGMn` | Copy `$MATCHED_DIR/assigned_year=Y_harvest.parquet` (from `apply_harvest.R`). Crop-specific fraction of biomass removed vs residue. |
| Tillage | `min_date` (most negative Delta-NDTI in the fallow window) | Copy `$PRODUCTS_INVENTORY/tillage/tillage_metrics/assigned_year=Y_tillage.parquet` (from `apply_tillage.R`). `tillage_eff_0to1` + date. Metrics method: [tillage/README.md](../tillage/README.md). |

## Parameters and flags

| Name | Role |
|------|------|
| `MATCHED_DIR` | Overlay + planting/harvest apply tables |
| `EVENT_OUTPUT_DIR` | Event parquet/JSON |
| `GAPFILL_DATES_DIR` | Gap-filled overlay (default `$MATCHED_DIR/gapfill_dates`) |
| `PLANT_TRAITS_DIR` | Lookups used by apply scripts (not re-read at event copy) |

| File under `$EVENT_OUTPUT_DIR` | SIPNET contents |
|------|-----------------|
| `assigned_year={year}_phenology.parquet` / `.json` | `leafon` / `leafoff` |
| `assigned_year={year}_planting.parquet` / `.json` | C and N pools (`crop_code`, leaf/wood/root) |
| `assigned_year={year}_harvest.parquet` / `.json` | Removal vs residue fractions |
| `assigned_year={year}_tillage.parquet` / `.json` | `date`, `tillage_eff_0to1` |

Event-file dictionaries (SIPNET columns only): [data/planting_statewide_metadata.csv](data/planting_statewide_metadata.csv), [harvest](data/harvest_statewide_metadata.csv), [phenology](data/phenology_statewide_metadata.csv), [tillage](data/tillage_statewide_metadata.csv). Apply-table dictionaries are linked in Assumptions.

`combine_management_events_pecan.R` merges planting/harvest/tillage/irrigation CSVs into one JSON bundle. That is not the statewide assigned pipeline.

| File | Role |
|------|------|
| `make_events_statewide.R` / `.sh` | CLI orchestrator |
| `R/matched_input.R` | Load overlay, filter candidates |
| `R/phenology_events.R` | Format leafon / leafoff |
| `R/planting_events.R` | Copy planting SIPNET columns |
| `R/harvest_events.R` | Copy harvest SIPNET columns |
| `R/tillage_events.R` | Copy tillage SIPNET columns |
| `R/tillage_metrics.R` | Fallow-window NDTI (`apply_tillage.R`) |
| `R/io.R` | Parquet + PEcAn JSON |
