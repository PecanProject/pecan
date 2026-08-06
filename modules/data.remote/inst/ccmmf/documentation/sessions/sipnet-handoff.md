# Appendix - SIPNET handoff (unofficial)

**Not a formal Session 4.** This appendix documents how CCMMF Management Tracking
event files become drivers for SIPNET through PEcAn. Monitoring
(`inst/ccmmf`) **produces** statewide event parquet; model-ready formatting
lives in **PEcAn `data.land` / SIPNET** and related restart workflows.

**Deliverable:** PEcAn `events.json` (and SIPNET `events.in`) from monitoring
outputs for MAGIC inventory runs.

**Spine:** [pipeline.md](../pipeline.md). Upstream sessions:
[2 - HLS events](02-phenology.md),
[3 - Fertilization and irrigation](03-fertilizer-irrigation.md).

---

## Why this step exists

MAGIC annual inventory and scenario runs need agronomic events in the formats
SIPNET and PEcAn expect. Monitoring writes parcel-year parquet under
`$CCMMF_MANAGEMENT/event_files/`. Those columns are convenient for the
monitoring pipeline; they are not always identical to the PEcAn events schema.
This handoff:

1. Cleans and renames columns (naming drift).
2. Builds validated `events.json`.
3. Writes SIPNET `events.in` via `PEcAn.SIPNET::write.events.SIPNET`.

Do not re-derive planting/harvest/tillage dates here.

---

## Input contract (monitoring)

Typical products from Sessions 2-3 (paths relative to `$CCMMF_MANAGEMENT`):

| Product | Example path pattern |
|---------|----------------------|
| Planting | `event_files/planting_statewide_Y.parquet` |
| Harvest | `event_files/harvest_statewide_Y.parquet` |
| Phenology | `event_files/phenology_statewide_Y.parquet` |
| Tillage | `event_files/tillage_statewide_Y.parquet` |
| Irrigation | From Session 3 / irrigation-statewide (separate layout) |

Column dictionaries: [metadata.md](../metadata.md), [events/README.md](../../events/README.md).

---

## Stage 1 - Clean and rename

Operator entry (lab reference; may live on a SIPNET restart workflow branch,
not inside `inst/ccmmf`):

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

**Tillage intensity helper on this monitoring branch:**
`PEcAn.data.land::ndti_to_sipnet_tillage()` maps fractional NDTI drop to
`tillage_eff_0to1` for the PEcAn events schema
(`modules/data.land/R/ndti_to_sipnet_tillage.R`).

**Residual risk:** some clean scripts still hard-code lab absolute paths and
assume current monitoring column names. Re-run after any event-schema change.
Prefer env-based paths when adapting the scripts for portable sites.

---

## Stage 2 - Parquet to `events.json`

On lines that ship `event_parquet_to_json` (for example the SIPNET restart
workflow / matching `PEcAn.data.land` version):

| Piece | Role |
|-------|------|
| `get_event_ensemble_ids()` | Ensemble IDs from parquet |
| `event_parquet_to_json()` | Manifest-driven `events.json` |
| Example driver | `workflows/preprocess-event-parquet/02-events-to-json.R` |

Validate against the events JSON schema before modeling:

| Piece | Role |
|-------|------|
| `PEcAn.data.land::validate_events()` | Schema validation helper on this branch |
| `modules/data.land/inst/events_schema_*.json` | Schema files (version as installed) |

Confirm the function names and schema version in **your** installed PEcAn
packages; package surfaces differ slightly across branches.

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

---

## Ownership

| Layer | Owner |
|-------|--------|
| Statewide monitoring parquet | `inst/ccmmf` Sessions 2-3 |
| Clean / rename / JSON / SIPNET write | PEcAn `data.land`, `PEcAn.SIPNET`, restart / preprocess workflows |

Do not duplicate conversion logic into `inst/ccmmf`; link and document.

---

## Checklist

- [ ] Monitoring `*_statewide_Y.parquet` present for required event types
- [ ] Clean scripts run; renamed columns match PEcAn schema expectations
- [ ] `tillage_eff_0to1` present when tillage is included (`ndti_to_sipnet_tillage` or clean mapping)
- [ ] `events.json` validated
- [ ] `write.events.SIPNET` produces `events.in` for target sites
- [ ] If clean scripts failed: check path hard-codes and column drift vs [events/README.md](../../events/README.md)

**Spine:** [pipeline.md](../pipeline.md).
