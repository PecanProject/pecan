# CCMMF workflow review plan (step by step)

**Goal:** Walk the inventory workflow from start to finish. At each step,
review **documentation and code together**.

**Canonical tree:** [`inst/ccmmf`](..) (this plan is under `documentation/`)

**Findings:** [`workflow_code_review_findings.md`](workflow_code_review_findings.md)

**Out of scope:** refactoring, package lifts, pushes.

Links are **relative to this file** (Cmd/Ctrl-click in the editor).

---

## How each step works

1. Read listed **docs** first.
2. Read listed **code** in order.
3. Check claims vs behavior; confusing names; silent skips; duplication; missing *why*; overcomplication.
4. Append to the findings log; check the box when done.

### Full spine

```mermaid
flowchart TD
  S0[1.0 Setup] --> S1[1.1 Legend]
  S1 --> S2[1.2 Harmonize]
  S2 --> S3[1.3-1.4 LandIQ gap-fill]
  S3 --> H[2.1 HLS tile map]
  H --> M[2.2 MSLSP extract]
  M --> MT[3.1 Match]
  MT --> GF[3.2 Date gap-fill]
  GF --> T[4.1 Traits]
  T --> E[4.2 Events P/H/Ph]
  H --> N[5.1 NDTI]
  N --> TG[5.2 Tillage events]
  E --> B[6.x Boundaries]
  TG --> B
```

---

## Block 1 -- Setup through LandIQ gap-fill


### Step 1.0 -- Map and setup

**Schematic**

```mermaid
flowchart LR
  subgraph docs [Docs]
    P[pipeline.md]
    R0[sessions/00-setup.md]
    RR[inst/ccmmf README]
  end
  subgraph code [Code]
    SE[setup_env.sh]
  end
  P --> SE
  R0 --> SE
  SE -->|exports env| ENV[Shell env for later stages]
```

**Docs**

- [ ] [`documentation/pipeline.md`](pipeline.md) (spine + maturity; skim known gaps)
- [ ] [`documentation/sessions/00-setup.md`](sessions/00-setup.md)
- [ ] [`README.md`](../README.md) (what this tree claims to cover)

**Code**

- [ ] [`documentation/setup_env.sh`](setup_env.sh)

**Check:** Env vars in Session 0 / setup_env vs names used later.

### Step 1.1 -- LandIQ download + legend

**Schematic**

```mermaid
flowchart LR
  S1[01-landiq.md download/legend] --> LUT[LandIQ_cropCode_lookup_table.csv]
  LUT --> GF[Gap-fill inputs]
```

**Docs**

- [ ] [`documentation/sessions/01-landiq.md`](sessions/01-landiq.md) (through legend QC / download)

**Code**

- [ ] [`landiq-gapfill/data/LandIQ_cropCode_lookup_table.csv`](../landiq-gapfill/data/LandIQ_cropCode_lookup_table.csv) (columns / ownership vs management copy)

**Check:** Download/legend match gap-fill inputs; no invented in-tree download orchestrator.

### Step 1.2 -- Geometry harmonization contract

**Schematic**

```mermaid
flowchart LR
  S1[01-landiq.md harmonize] --> EXT[cadwr-landuse external]
  EXT --> GPKG["$LANDIQ_HARMONIZED\nparcels-consolidated.gpkg"]
  EXT --> PARQ["$LANDIQ_HARMONIZED\ncrops_all_years.parq"]
  PARQ --> GF[landiq-gapfill]
  GF --> V4["$LANDIQ_GAPFILLED\ncrops_all_years.parq"]
  LR[landiq-gapfill README] --> V4
```

**Docs**

- [ ] [`documentation/sessions/01-landiq.md`](sessions/01-landiq.md) (harmonize / cadwr-landuse)
- [ ] [`landiq-gapfill/README.md`](../landiq-gapfill/README.md) (product inputs)

**Code**

- None in this clone (external `cadwr-landuse`). Confirm harmonized `parcels-consolidated.gpkg` + `crops_all_years.parq`, and gap-filled `$LANDIQ_GAPFILLED/crops_all_years.parq` (geometry stays under harmonized).

**Check:** Docs name the right paths/filenames; gap-fill scripts agree. Geometry is not copied/symlinked into gapfilled.

### Step 1.3 -- LandIQ gap-fill orchestration + CDL

**Schematic**

```mermaid
flowchart TD
  DL[download_cdl_nass.R] --> TIF[CDL .tif]
  TIF --> EX[extract_cdl_fractions_by_parcel.R]
  EX --> FRAC[cdl_fractions parquet]
  FRAC --> RUN[run_gapfill.sh]
  RUN --> GF[gapfill.R]
  GF --> CROP[crop YEAR]
  GF --> ADOY[adoy YEAR]
  GF --> PROD[merge]
  GF --> QC[qc]
  GF -.->|occasional| EM[cdl-landiq-probs / adoy-ref]
```

**Docs**

- [ ] [`landiq-gapfill/README.md`](../landiq-gapfill/README.md) (full run path)
- [ ] [`documentation/sessions/01-landiq.md`](sessions/01-landiq.md) (gap-fill / CDL sections)

**Code**

- [ ] [`landiq-gapfill/run_gapfill.sh`](../landiq-gapfill/run_gapfill.sh)
- [ ] [`landiq-gapfill/scripts/gapfill.R`](../landiq-gapfill/scripts/gapfill.R)
- [ ] [`landiq-gapfill/scripts/cdl/download_cdl_nass.R`](../landiq-gapfill/scripts/cdl/download_cdl_nass.R)
- [ ] [`landiq-gapfill/scripts/cdl/extract_cdl_fractions_by_parcel.R`](../landiq-gapfill/scripts/cdl/extract_cdl_fractions_by_parcel.R)

### Step 1.4 -- LandIQ gap-fill libraries

**Schematic**

```mermaid
flowchart TD
  CFG[gapfill_config + paths + bootstrap] --> RUN[gapfill_run / cli]
  RUN --> CLASS[gapfill_class]
  RUN --> SUB[gapfill_subclass]
  RUN --> AD[gapfill_adoy]
  CDL[gapfill_cdl] --> SUB
  EM[gapfill_emission] -.->|not routine| SUB
  CT[county_transition] --> CLASS
  RUN --> BLD[build_landiq_product]
  BLD --> COVER[cover_crop_landiq]
  BLD --> QC[qc_gapfill_product]
```

**Docs**

- [ ] [`landiq-gapfill/README.md`](../landiq-gapfill/README.md) (re-skim provenance / season-2)
- [ ] [`landiq-gapfill/outputs/qc_gapfill_report.md`](../landiq-gapfill/outputs/qc_gapfill_report.md) (skim claims only)

**Code**

- [ ] [`landiq-gapfill/scripts/R/bootstrap.R`](../landiq-gapfill/scripts/R/bootstrap.R)
- [ ] [`landiq-gapfill/scripts/R/pkg_root.R`](../landiq-gapfill/scripts/R/pkg_root.R)
- [ ] [`landiq-gapfill/scripts/R/paths.R`](../landiq-gapfill/scripts/R/paths.R)
- [ ] [`landiq-gapfill/scripts/R/lookup_paths.R`](../landiq-gapfill/scripts/R/lookup_paths.R)
- [ ] [`landiq-gapfill/scripts/R/gapfill_cli.R`](../landiq-gapfill/scripts/R/gapfill_cli.R)
- [ ] [`landiq-gapfill/scripts/R/gapfill_config.R`](../landiq-gapfill/scripts/R/gapfill_config.R)
- [ ] [`landiq-gapfill/scripts/R/gapfill_run.R`](../landiq-gapfill/scripts/R/gapfill_run.R)
- [ ] [`landiq-gapfill/scripts/R/gapfill_class.R`](../landiq-gapfill/scripts/R/gapfill_class.R)
- [ ] [`landiq-gapfill/scripts/R/gapfill_subclass.R`](../landiq-gapfill/scripts/R/gapfill_subclass.R)
- [ ] [`landiq-gapfill/scripts/R/gapfill_adoy.R`](../landiq-gapfill/scripts/R/gapfill_adoy.R)
- [ ] [`landiq-gapfill/scripts/R/gapfill_cdl.R`](../landiq-gapfill/scripts/R/gapfill_cdl.R)
- [ ] [`landiq-gapfill/scripts/R/gapfill_emission.R`](../landiq-gapfill/scripts/R/gapfill_emission.R)
- [ ] [`landiq-gapfill/scripts/R/gapfill_lookup_build.R`](../landiq-gapfill/scripts/R/gapfill_lookup_build.R)
- [ ] [`landiq-gapfill/scripts/R/gapfill_lookup_probs.R`](../landiq-gapfill/scripts/R/gapfill_lookup_probs.R)
- [ ] [`landiq-gapfill/scripts/R/landiq_rs_harmonize.R`](../landiq-gapfill/scripts/R/landiq_rs_harmonize.R)
- [ ] [`landiq-gapfill/scripts/R/county_transition.R`](../landiq-gapfill/scripts/R/county_transition.R)
- [ ] [`landiq-gapfill/scripts/R/build_landiq_product.R`](../landiq-gapfill/scripts/R/build_landiq_product.R)
- [ ] [`landiq-gapfill/scripts/R/cover_crop_landiq.R`](../landiq-gapfill/scripts/R/cover_crop_landiq.R)
- [ ] [`landiq-gapfill/scripts/R/qc_gapfill_product.R`](../landiq-gapfill/scripts/R/qc_gapfill_product.R)

**Check:** Season-2-only, COVER, provenance, no routine emission rebuild match code.

## Block 2 -- HLS + MSLSP extract


### Step 2.1 -- HLS parcel-tile map

**Schematic**

```mermaid
flowchart TD
  EXT[build_hls_tile_extent.R] --> MAP[build_hls_parcel_tile_map.R]
  LIB[hls/R parcel_tilemap + tilewise_core] --> MAP
  MAP --> CSV[hls_parcel_tile_map_v4.1.csv]
  RDS --> MSLSP[MSLSP extract]
  RDS --> NDTI[NDTI extract]
```

**Docs**

- [ ] [`documentation/sessions/02-phenology.md`](sessions/02-phenology.md) (HLS / tile-map)
- [ ] [`hls/README.md`](../hls/README.md)
- [ ] [`documentation/pipeline.md`](pipeline.md) (HLS step if listed)

**Code**

- [ ] [`hls/build_hls_tile_extent.R`](../hls/build_hls_tile_extent.R)
- [ ] [`hls/build_hls_parcel_tile_map.R`](../hls/build_hls_parcel_tile_map.R)
- [ ] [`hls/R/bootstrap.R`](../hls/R/bootstrap.R)
- [ ] [`hls/R/pkg_root.R`](../hls/R/pkg_root.R)
- [ ] [`hls/R/parcel_tilemap.R`](../hls/R/parcel_tilemap.R)
- [ ] [`hls/R/tilewise_core.R`](../hls/R/tilewise_core.R)
- [ ] [`hls/R/extract_summary_core.R`](../hls/R/extract_summary_core.R)

### Step 2.2 -- MSLSP extract

**Schematic**

```mermaid
flowchart TD
  SH[run_mslsp.sh] --> PREP[prep_static.R]
  PREP --> EXT[extract_tiles / task]
  EXT --> IMP[tilewise_mslsp_implementation]
  CORE[hls/R/tilewise_core] --> IMP
  EXT --> COMB[combine_year.R]
  COMB --> OUT[raw_mslsp_v4.1.2 parquet]
```

**Docs**

- [ ] [`phenology/README.md`](../phenology/README.md)
- [ ] [`phenology/extract/README.md`](../phenology/extract/README.md)
- [ ] [`documentation/sessions/02-phenology.md`](sessions/02-phenology.md) (MSLSP extract)

**Code**

- [ ] [`phenology/run_mslsp.sh`](../phenology/run_mslsp.sh)
- [ ] [`phenology/extract/scripts/prep_static.R`](../phenology/extract/scripts/prep_static.R)
- [ ] [`phenology/extract/scripts/extract_tiles.R`](../phenology/extract/scripts/extract_tiles.R)
- [ ] [`phenology/extract/scripts/extract_tiles_task.R`](../phenology/extract/scripts/extract_tiles_task.R)
- [ ] [`phenology/extract/scripts/combine_year.R`](../phenology/extract/scripts/combine_year.R)
- [ ] [`phenology/extract/scripts/R/bootstrap.R`](../phenology/extract/scripts/R/bootstrap.R)
- [ ] [`phenology/extract/scripts/R/pkg_root.R`](../phenology/extract/scripts/R/pkg_root.R)
- [ ] [`phenology/extract/scripts/R/paths.R`](../phenology/extract/scripts/R/paths.R)
- [ ] [`phenology/extract/scripts/R/mslsp_cli.R`](../phenology/extract/scripts/R/mslsp_cli.R)
- [ ] [`phenology/extract/scripts/R/mslsp_run.R`](../phenology/extract/scripts/R/mslsp_run.R)
- [ ] [`phenology/extract/scripts/R/mslsp_combine.R`](../phenology/extract/scripts/R/mslsp_combine.R)
- [ ] [`phenology/extract/scripts/R/tilewise_mslsp_implementation.R`](../phenology/extract/scripts/R/tilewise_mslsp_implementation.R)

**Check:** Top-2 cycles, NetCDF layout, `raw_mslsp_v4.1.2`; overlap with `hls/R/tilewise_core.R`.

## Block 3 -- Match + date gap-fill


### Step 3.1 -- LandIQ-MSLSP match

**Schematic**

```mermaid
flowchart LR
  LIQ[LandIQ product] --> M[match_landiq_mslsp.R]
  MS[raw MSLSP] --> M
  M --> ASG[assigned_year=Y.parquet]
  M --> QC[build_qc_report.R]
  ASG --> EV[Events + date gap-fill]
```

**Docs**

- [ ] [`phenology/match/README.md`](../phenology/match/README.md)
- [ ] [`documentation/sessions/02-phenology.md`](sessions/02-phenology.md) (match section)

**Code**

- [ ] [`phenology/match_landiq_mslsp.sh`](../phenology/match_landiq_mslsp.sh)
- [ ] [`phenology/match/matched_paths.R`](../phenology/match/matched_paths.R)
- [ ] [`phenology/match/match_landiq_mslsp.R`](../phenology/match/match_landiq_mslsp.R)
- [ ] [`phenology/match/build_qc_report.R`](../phenology/match/build_qc_report.R)

**Check:** `assigned_by` / `no_mslsp` / QC vs what events later use.

### Step 3.2 -- Phenology date gap-fill

**Schematic**

```mermaid
flowchart TD
  FIT[fit_phenology_gapfill_models.R] --> RDS[gapfill models rds]
  RDS --> APL[apply_phenology_gapfill.R]
  ASG[assigned_year=Y.parquet] --> APL
  APL --> OVL[gapfill_dates overlay parquet]
  OVL -->|gapfill_date_source| EV[Event builders]
  AUD[gapfill_phase0_audit] -.-> FIT
```

**Docs**

- [ ] [`phenology/gapfill/README.md`](../phenology/gapfill/README.md)
- [ ] [`documentation/sessions/02-phenology.md`](sessions/02-phenology.md) (date gap-fill)
- [ ] [`documentation/pipeline.md`](pipeline.md) (required before statewide planting/harvest)

**Code**

- [ ] [`phenology/run_phenology_date_gapfill.sh`](../phenology/run_phenology_date_gapfill.sh)
- [ ] [`phenology/gapfill/fit_phenology_gapfill_models.R`](../phenology/gapfill/fit_phenology_gapfill_models.R)
- [ ] [`phenology/gapfill/apply_phenology_gapfill.R`](../phenology/gapfill/apply_phenology_gapfill.R)
- [ ] [`phenology/gapfill/gapfill_phase0_audit.sh`](../phenology/gapfill/gapfill_phase0_audit.sh)
- [ ] [`phenology/gapfill/gapfill_phase0_audit.R`](../phenology/gapfill/gapfill_phase0_audit.R)

**Check:** Overlay path, `gapfill_date_source`, does not overwrite canonical assigned parquet.

## Block 4 -- Traits + events


### Step 4.1 -- Trait lookups

**Schematic**

```mermaid
flowchart TD
  TRY[TRY + literature sources] --> PL[build_planting_lookup.R]
  HF[harvest fractions long] --> HV[build_harvest_lookup.R]
  PL --> CSV1[planting_lookup.csv]
  HV --> CSV2[harvest_lookup.csv]
  CSV1 --> POOL[pool_calculations_from_lookup.R]
  CSV2 --> POOL
  LAI[lai_from_mslsp.R] --> POOL
  POOL --> EV[Planting/harvest events]
```

**Docs**

- [ ] [`traits/README.md`](../traits/README.md)
- [ ] [`documentation/sessions/02-phenology.md`](sessions/02-phenology.md) (traits blurb)
- [ ] [`documentation/pipeline.md`](pipeline.md) (traits if listed)

**Code**

- [ ] [`traits/build_planting_lookup.R`](../traits/build_planting_lookup.R)
- [ ] [`traits/build_harvest_lookup.R`](../traits/build_harvest_lookup.R)
- [ ] [`traits/write_harvest_fractions_long.R`](../traits/write_harvest_fractions_long.R)
- [ ] [`traits/lai_from_mslsp.R`](../traits/lai_from_mslsp.R)
- [ ] [`traits/pool_calculations_from_lookup.R`](../traits/pool_calculations_from_lookup.R)

**Check:** Fallback order, destructive woody, LAI `k=0.15` vs MSLSP date defs.

### Step 4.2 -- Statewide events (phenology / planting / harvest)

**Schematic**

```mermaid
flowchart TD
  SH[make_events_statewide.sh] --> ORCH[make_events_statewide.R]
  OVL[gapfilled assigned] --> MI[matched_input.R]
  MI --> PH[phenology_events.R]
  MI --> PL[planting_events.R]
  MI --> HV[harvest_events.R]
  POOL[trait_pool + lookups] --> PL
  POOL --> HV
  PH --> IO[io.R parquet+JSON]
  PL --> IO
  HV --> IO
```

**Docs**

- [ ] [`events/README.md`](../events/README.md)
- [ ] [`documentation/sessions/02-phenology.md`](sessions/02-phenology.md) (events section)
- [ ] [`events/data/planting_statewide_metadata.csv`](../events/data/planting_statewide_metadata.csv)
- [ ] [`events/data/harvest_statewide_metadata.csv`](../events/data/harvest_statewide_metadata.csv)
- [ ] [`events/data/phenology_statewide_metadata.csv`](../events/data/phenology_statewide_metadata.csv)
- [ ] [`documentation/metadata.md`](metadata.md)

**Code**

- [ ] [`events/make_events_statewide.sh`](../events/make_events_statewide.sh)
- [ ] [`events/make_events_statewide.R`](../events/make_events_statewide.R)
- [ ] [`events/R/bootstrap.R`](../events/R/bootstrap.R)
- [ ] [`events/R/paths.R`](../events/R/paths.R)
- [ ] [`events/R/matched_input.R`](../events/R/matched_input.R)
- [ ] [`events/R/phenology_events.R`](../events/R/phenology_events.R)
- [ ] [`events/R/planting_events.R`](../events/R/planting_events.R)
- [ ] [`events/R/harvest_events.R`](../events/R/harvest_events.R)
- [ ] [`events/R/trait_pool.R`](../events/R/trait_pool.R)
- [ ] [`events/R/io.R`](../events/R/io.R)
- [ ] [`events/combine_management_events_pecan.R`](../events/combine_management_events_pecan.R)

**Check:** Provenance columns, YP skip, woody destructive, JSON vs parquet vs metadata.

## Block 5 -- NDTI + tillage events


### Step 5.1 -- NDTI extract

**Schematic**

```mermaid
flowchart TD
  SH[run_ndti.sh] --> PREP[prep_static.R]
  PREP --> EXT[extract_tiles.R]
  EXT --> IMP[tilewise_ndti_implementation]
  CORE[hls/R/tilewise_core] --> IMP
  EXT --> COMB[combine_month.R]
  COMB --> OUT[ndti_v4.1 monthly parquet]
```

**Docs**

- [ ] [`tillage/README.md`](../tillage/README.md)
- [ ] [`tillage/extract/README.md`](../tillage/extract/README.md)
- [ ] [`documentation/sessions/02-phenology.md`](sessions/02-phenology.md) (NDTI / tillage)

**Code**

- [ ] [`tillage/run_ndti.sh`](../tillage/run_ndti.sh)
- [ ] [`tillage/extract/scripts/prep_static.R`](../tillage/extract/scripts/prep_static.R)
- [ ] [`tillage/extract/scripts/extract_tiles.R`](../tillage/extract/scripts/extract_tiles.R)
- [ ] [`tillage/extract/scripts/combine_month.R`](../tillage/extract/scripts/combine_month.R)
- [ ] [`tillage/extract/scripts/R/bootstrap.R`](../tillage/extract/scripts/R/bootstrap.R)
- [ ] [`tillage/extract/scripts/R/pkg_root.R`](../tillage/extract/scripts/R/pkg_root.R)
- [ ] [`tillage/extract/scripts/R/paths.R`](../tillage/extract/scripts/R/paths.R)
- [ ] [`tillage/extract/scripts/R/ndti_cli.R`](../tillage/extract/scripts/R/ndti_cli.R)
- [ ] [`tillage/extract/scripts/R/ndti_run.R`](../tillage/extract/scripts/R/ndti_run.R)
- [ ] [`tillage/extract/scripts/R/ndti_combine.R`](../tillage/extract/scripts/R/ndti_combine.R)
- [ ] [`tillage/extract/scripts/R/tilewise_ndti_implementation.R`](../tillage/extract/scripts/R/tilewise_ndti_implementation.R)

**Check:** Imagery layout env; duplication vs phenology extract / `hls/R`.

### Step 5.2 -- Tillage events + lookback

**Schematic**

```mermaid
flowchart TD
  NDTI[NDTI hive] --> TE[tillage_events.R]
  PHENO[matched phenology windows] --> TM[tillage_metrics.R]
  TM --> TE
  TE --> OUT[tillage_statewide_Y]
  TE --> LB[lookback amend parquets]
  LB --> MG[merge_tillage_lookback]
  MG --> OUT
```

**Docs**

- [ ] [`events/README.md`](../events/README.md) (tillage algorithm)
- [ ] [`events/data/tillage_statewide_metadata.csv`](../events/data/tillage_statewide_metadata.csv)
- [ ] [`documentation/sessions/02-phenology.md`](sessions/02-phenology.md) (tillage events)

**Code**

- [ ] [`events/R/tillage_metrics.R`](../events/R/tillage_metrics.R)
- [ ] [`events/R/tillage_events.R`](../events/R/tillage_events.R)
- [ ] [`events/merge_tillage_lookback.sh`](../events/merge_tillage_lookback.sh)
- [ ] [`events/merge_tillage_lookback.R`](../events/merge_tillage_lookback.R)
- [ ] [`tillage/smoke_tillage_metrics_year.R`](../tillage/smoke_tillage_metrics_year.R)

**Check:** Buffer years, lookback amend merge, provenance fields.

## Block 6 -- Session 3 + SIPNET boundaries


### Step 6.1 -- Fert / irrigation (boundary)

**Schematic**

```mermaid
flowchart LR
  S3[03-fertilizer-irrigation.md] --> LAND[PEcAn.data.land lookups]
  S3 --> WF[workflows fert/NCC/irrigation]
  S3 -.->|not on this branch| MISSING[No inst/ccmmf builders]
```

**Docs**

- [ ] [`documentation/sessions/03-fertilizer-irrigation.md`](sessions/03-fertilizer-irrigation.md)
- [ ] [`documentation/pipeline.md`](pipeline.md) (fert / irrigation claims)
- [ ] [`README.md`](../README.md) (coverage claims)
- [ ] [`events/README.md`](../events/README.md) (Session 3 pointers)

**Code**

- Boundary check only (no statewide fert/NCC scripts on this monitoring branch).

**Check:** No dead in-tree fert/NCC builders; pointers to PEcAn.data.land / workflows / PR #4003 only.

### Step 6.2 -- SIPNET handoff (boundary)

**Schematic**

```mermaid
flowchart LR
  EV[event_files statewide parquet] --> EXT[preprocess-event-parquet external]
  EXT --> JSON[events.json]
  JSON --> SIP[SIPNET events.in]
  DOC[sipnet-handoff.md] -.-> EXT
```

**Docs**

- [ ] [`documentation/sessions/sipnet-handoff.md`](sessions/sipnet-handoff.md)
- [ ] [`documentation/pipeline.md`](pipeline.md) (appendix if any)

**Code**

- Boundary check only.

**Check:** Monitoring stops at statewide event parquet/JSON; conversion outside `inst/ccmmf`.

### Step 6.3 -- Close-out

**Schematic**

```mermaid
flowchart LR
  LOG[findings log] --> TRIAGE[Triage H then M]
  TRIAGE --> DRIFT[Note mgmt vs pecan drift]
  TRIAGE --> ENV[Optional env-var grep vs setup_env]
```

- [ ] Triage [`workflow_code_review_findings.md`](workflow_code_review_findings.md) (H first)
- [ ] Note management-vs-pecan drift for a later sync
- [ ] Optional: env-var grep vs [`documentation/setup_env.sh`](setup_env.sh)
