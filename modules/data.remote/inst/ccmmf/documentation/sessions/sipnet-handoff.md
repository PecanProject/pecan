# Appendix - SIPNET handoff (unofficial)

**Not a formal Session 4.** This appendix documents how CCMMF Management Tracking
event files become drivers for SIPNET through PEcAn. Monitoring
(`inst/ccmmf`) **produces** statewide event parquet; model-ready formatting
lives in **PEcAn `data.land` / SIPNET** and related restart workflows.

**Deliverable:** PEcAn `events.json` (and SIPNET `events.in`) from monitoring
outputs for MAGIC inventory runs.

**Spine:** [tree README](../../README.md). Upstream sessions:
[2 - HLS events](02-phenology.md),
[3 - Fertilization and irrigation](03-fertilizer-irrigation.md).

---

## Why this step exists

MAGIC annual inventory and scenario runs need agronomic events in the formats
SIPNET and PEcAn expect. Monitoring writes parcel-year parquet under
`$PRODUCTS_INVENTORY/event_files/`. Those columns are convenient for the
monitoring pipeline; they are not always identical to the PEcAn events schema.
This handoff:

1. Cleans and renames columns (naming drift).
2. Builds schema-checked `events.json`.
3. Writes SIPNET `events.in` via `PEcAn.SIPNET::write.events.SIPNET`.

Do not re-derive planting/harvest/tillage dates here.

---

## Input contract (monitoring)

Typical products from Sessions 2-3 (paths relative to `$PRODUCTS_INVENTORY`):

| Product | Example path pattern |
|---------|----------------------|
| Planting | `event_files/planting_statewide_Y.parquet` |
| Harvest | `event_files/harvest_statewide_Y.parquet` |
| Phenology | `event_files/phenology_statewide_Y.parquet` |
| Tillage | `event_files/tillage_statewide_Y.parquet` |
| Irrigation | From Session 3 / irrigation-statewide (separate layout) |

Column dictionaries: [metadata.md](../metadata.md), [events/README.md](../../events/README.md).

---

## Where the stages live

| Stage | On this monitoring tree | Elsewhere (SIPNET restart line) |
|-------|-------------------------|----------------------------------|
| Clean / rename parquet | -- | `workflows/preprocess-event-parquet/` |
| Parquet to `events.json` | -- | same workflow + `event_parquet_to_json()` in that line's `PEcAn.data.land` |
| Schema check | `validate_events_json()` when present in your installed `data.land` | same |
| SIPNET `events.in` | `PEcAn.SIPNET::write.events.SIPNET` | also used from restart drivers |
| Restart / run drivers | `workflows/sipnet-restart-workflow/` (prepare settings, run) | fuller checkout may include preprocess beside restart |

Stages 1-2 below are **not** under `workflows/` on this monitoring branch. Use a
SIPNET restart workflow checkout that includes
`workflows/preprocess-event-parquet/` (lab reference: ashiklom
`sipnet-restart-workflow` tree). This appendix keeps the column map and the
monitoring-side helpers that *are* here.

---

## Stage 1 - Clean and rename

Operator entry on the SIPNET restart line:

`workflows/preprocess-event-parquet/`

| Script | Role |
|--------|------|
| `01b-clean-other-events.R` | Planting, harvest, phenology, tillage |
| `01a-clean-irrigation.R` | Irrigation (often handled separately; large) |
| `README.md` | Workflow overview |

Example renames and transforms used in `01b-clean-other-events.R` (verify
against the script version you run):

| Monitoring / raw | Cleaned / PEcAn-oriented |
|------------------|--------------------------|
| `code` | `crop_code` |
| `C_LEAF`, `C_STEM`, `C_FINEROOT`, `C_COARSEROOT` | `leaf_c_kg_m2`, `wood_c_kg_m2`, `fine_root_c_kg_m2`, `coarse_root_c_kg_m2` |
| `N_LEAF`, ... | `leaf_n_kg_m2`, ... |
| `ndti_pct_change` | `tillage_eff_0to1` (mapped; see tillage helper) |
| Phenology `leafonday` / `leafoffday` | Separate `leafon` / `leafoff` parquet with `site_id`, `date` |

**Tillage intensity helper available with monitoring / shared `data.land`:**
`PEcAn.data.land::ndti_to_sipnet_tillage()` maps fractional NDTI drop to
`tillage_eff_0to1` for the PEcAn events schema
(`modules/data.land/R/ndti_to_sipnet_tillage.R`).

Re-run clean scripts after any monitoring event-schema change. Prefer env-based
paths when adapting scripts for portable sites.

---

## Stage 2 - Parquet to `events.json`

On the same SIPNET restart line (and matching `PEcAn.data.land` there):

| Piece | Role |
|-------|------|
| `get_event_ensemble_ids()` | Ensemble IDs from parquet |
| `event_parquet_to_json()` | Manifest-driven `events.json` |
| Example driver | `workflows/preprocess-event-parquet/02-events-to-json.R` |

Schema-check before modeling (function name on this monitoring tree's
`data.land` when installed):

| Piece | Role |
|-------|------|
| `PEcAn.data.land::validate_events_json()` | JSON schema check |
| `modules/data.land/inst/events_schema_*.json` | Schema files (version as installed) |

Confirm function names and schema version in **your** installed PEcAn packages;
surfaces differ across branches.

---

## Stage 3 - SIPNET `events.in`

```r
# After events.json exist:
PEcAn.SIPNET::write.events.SIPNET(events_json, outdir)
```

See `models/sipnet/R/write.events.SIPNET.R`. Related helpers you may need for
inventory workflows (not every run):

| Helper | Role |
|--------|------|
| `events_to_crop_cycle_starts()` | Planting-based cycle starts from events JSON |
| `mslsp_to_canopycover()` | Canopy cover from MSLSP metrics (when used) |

On this tree, `workflows/sipnet-restart-workflow/` covers prepare-settings / run
drivers once `events.json` exist.

---

## Ownership

| Layer | Owner |
|-------|--------|
| Statewide monitoring parquet | `inst/ccmmf` Sessions 2-3 |
| Clean / rename / parquet-to-JSON | SIPNET restart line: `workflows/preprocess-event-parquet/` |
| Schema check / SIPNET write | `PEcAn.data.land`, `PEcAn.SIPNET` |
| Restart drivers | `workflows/sipnet-restart-workflow/` |

Do not duplicate conversion logic into `inst/ccmmf`; link and document.

---

**Spine:** [tree README](../../README.md).
