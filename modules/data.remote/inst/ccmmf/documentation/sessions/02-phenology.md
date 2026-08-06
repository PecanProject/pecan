# Session 2 - HLS events (phenology and tillage)

**Deliverable:** parcel-level planting, harvest, phenology, and tillage
management event files for the year pair (MAGIC Management Tracking inputs
from HLS).

**Goal:** from Harmonized Landsat Sentinel-2 (HLS) products, build parcel-level
**planting**, **harvest**, **phenology**, and **tillage** management events for
the operational year pair (`TARGET_YEAR=2024`, `PRIOR_YEAR=2023`). Multi-Source
Land Surface Phenology (MSLSP) drives the first three; Normalized Difference
Tillage Index (NDTI) drives tillage in fallow windows.

**Method class:** hybrid RS + trait CSV lookups (planting/harvest); RS for
phenology and tillage. **Maturity:** operational (inventory); tillage build is opt-in.

**Prerequisite:** complete [Session 1](01-landiq.md). Point
`CCMMF_LANDIQ_V4` at `$CCMMF_LANDIQ_GAPFILL_PRODUCT`. Have NASA Earthdata
credentials from [Session 0](00-setup.md).

---

## Where you are

Same flow as [pipeline.md](../pipeline.md). This session is the HLS box.

```mermaid
flowchart TB
  subgraph S1["Session 1 - Crop identity"]
    DWR["LandIQ shapefile"] --> CADWR["Harmonize geometry"]
    CADWR --> GF["Gap-fill crops + ADOY"]
  end

  subgraph S2["Session 2 - HLS events - you are here"]
    HLS["HLS_Phenology\nNetCDF + imagery"] --> MSLSP["MSLSP extract"]
    GF --> MAP["Parcel-tile map"]
    MAP --> MSLSP
    GF --> MSLSP
    MSLSP --> MATCH["Match seasons to cycles"]
    GF --> MATCH
    MATCH --> EV1["Planting + harvest\n+ phenology events"]
    HLS --> NDTI["NDTI extract"]
    MAP --> NDTI
    GF --> NDTI
    NDTI --> EV2["Tillage events"]
    MATCH --> EV2
  end

  subgraph S3["Session 3 - Fert + irrigation"]
    FERT["N fert + organic"]
    IRR["Irrigation water-balance"]
  end

  EV1 --> OUT["Management event files"]
  EV2 --> OUT
  FERT --> OUT
  IRR --> OUT
```

This session = Session 2 box (MSLSP path, then NDTI / tillage).

**Demo vs statewide:** live path is one HLS tile (`10SDH`). Statewide omits
`TILEWISE_ONE_TILE` / `ASSIGN_PARCEL_IDS_FILE`.

**Operator docs** (algorithms and flags):

| Step | README |
|------|--------|
| Parcel-tile map + shared HLS helpers | [hls/README.md](../../hls/README.md) |
| MSLSP parcel extraction | [phenology/extract/README.md](../../phenology/extract/README.md) |
| LandIQ <-> MSLSP matching | [phenology/match/README.md](../../phenology/match/README.md) |
| Date gap-fill (required statewide) | [phenology/gapfill/README.md](../../phenology/gapfill/README.md) |
| Trait lookups | [traits/README.md](../../traits/README.md) |
| NDTI parcel extraction | [tillage/extract/README.md](../../tillage/extract/README.md) |
| Statewide events | [events/README.md](../../events/README.md) |
| Column dictionaries | [metadata.md](../metadata.md) |

---

## 2.1 Env and demo parcel list

```bash
source "$CCMMF_CODE/documentation/setup_env.sh"
export CCMMF_LANDIQ_V4=$CCMMF_LANDIQ_GAPFILL_PRODUCT
export DEMO_TILE=10SDH
export ASSIGN_PARCEL_IDS_FILE=$CCMMF_MANAGEMENT/demo/parcels_${DEMO_TILE}.csv
```

| Item | Path / format | Notes |
|------|---------------|--------|
| Input | `$CCMMF_LANDIQ_GAPFILL_PRODUCT/crops_all_years.parq` | Gap-filled LandIQ (Session 1) |
| Input | `$CCMMF_ROOT/data_phen/output/<tile>/phenoMetrics/MSLSP_<tile>_<year>.nc` | Tile MSLSP NetCDF ([HLS_Phenology](https://github.com/mrinareddy/HLS_Phenology)) |
| Output (once) | `$CCMMF_MANAGEMENT/hls_parcel_tile_map_v4.1.rds` | Parcel -> tiles; see [hls/README.md](../../hls/README.md) |
| Output (once) | `$CCMMF_MANAGEMENT/hls_tile_to_parcels_v4.1.rds` | Tile -> parcel ids |
| Demo list | `$CCMMF_MANAGEMENT/demo/parcels_10SDH.csv` | CSV header `parcel_id` |

Build the demo CSV after the tile map exists:

```r
tp <- readRDS(file.path(Sys.getenv("CCMMF_MANAGEMENT"), "hls_tile_to_parcels_v4.1.rds"))
tile <- "10SDH"
ids <- sort(unique(as.character(tp[[tile]])))
out <- file.path(Sys.getenv("CCMMF_MANAGEMENT"), "demo", paste0("parcels_", tile, ".csv"))
dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
write.csv(data.frame(parcel_id = ids), out, row.names = FALSE)
```

Parcel-tile map (once, when geometry changes):

```bash
Rscript "$CCMMF_CODE/hls/build_hls_parcel_tile_map.R" overwrite
```

---

## 2.2 MSLSP extract and match

LandIQ says *what* grows and peak greenness (**ADOY**). MSLSP gives satellite
green-up, peak, and senescence for up to two cycles per parcel-year.

### MSLSP timing (aligned with proposal 15% / 50% greenness)

MSLSP defines thresholds on the cycle EVI2 curve. We use those product metrics
directly; they implement the same greenness fractions the proposal named, under
MSLSP names:

| Metric | MSLSP meaning | Inventory use |
|--------|---------------|---------------|
| **OGI** | ~15% of peak greenness (onset of greenness) | Planting date |
| **50PCGI** | 50% of peak on green-up | Phenology leaf-on |
| **Peak** | Cycle peak | Match tie-break / year of phenology event |
| **50PCGD** | 50% of peak on green-down | Phenology leaf-off |
| **OGD / OGMn** | Senescence / onset of minimum (PFT rules) | Harvest date |

**Planting** is therefore an **RS effective plant**: canopy becoming visible
(~seedling / early greenness), not calendar seed-in-ground. That matches OGI
and is what satellite phenology can support statewide.

The extract keeps the **top two** MSLSP amplitude cycles per parcel-year. LandIQ
has up to four seasons, but seasons 1/3/4 are uncommon (see
[landiq-gapfill README](../../landiq-gapfill/README.md#data-model)); matching
extra cycles for those seasons is future work, not blocked by the product
format.

| Event type | Typical date source | What SIPNET gets |
|------------|---------------------|------------------|
| Phenology | MSLSP 50PCGI / 50PCGD | Leaf-on / leaf-off |
| Planting | MSLSP OGI (~15% of peak) | C/N pools (LAI from MSLSP EVI) |
| Harvest | MSLSP senescence (PFT-specific) | Biomass removal fractions |

**A. Extract + combine (demo tile)**

```bash
TILEWISE_ONE_TILE=$DEMO_TILE $PHENOLOGY_ROOT/run_mslsp.sh 2024
TILEWISE_ONE_TILE=$DEMO_TILE $PHENOLOGY_ROOT/run_mslsp.sh 2023
```

| Item | Path / format | Key columns / metadata |
|------|---------------|------------------------|
| Input | MSLSP NetCDF under `data_phen/output/` | Per-tile annual NetCDF |
| Output | `$CCMMF_MANAGEMENT/phenology/raw_mslsp_v4.1.2/year=Y/` | Hive parquet; [mslsp_year_metadata.csv](../../phenology/extract/data/mslsp_year_metadata.csv) |

Prefer `TILEWISE_ONE_TILE` (includes combine). Details:
[phenology/extract/README.md](../../phenology/extract/README.md).

**B. Match (demo parcels)**

```bash
ASSIGN_PARCEL_IDS_FILE=$ASSIGN_PARCEL_IDS_FILE \
  $PHENOLOGY_ROOT/match_landiq_mslsp.sh 2024
```

| Item | Path / format | Key columns / metadata |
|------|---------------|------------------------|
| Output | `phenology/matched_landiq_mslsp_v4.1.2/.../assigned_year=2024.parquet` | `assigned_by`, `match_outcome`; [assigned_year_metadata.csv](../../phenology/match/data/assigned_year_metadata.csv) |

**C. Trait lookups (once)** and **events**

```bash
# If missing under $CCMMF_MANAGEMENT/plant_traits/:
Rscript "$CCMMF_CODE/traits/build_planting_lookup.R"
Rscript "$CCMMF_CODE/traits/build_harvest_lookup.R"

export CCMMF_MATCHED_DIR=$CCMMF_MANAGEMENT/phenology/matched_landiq_mslsp_v4.1.2/subsample_n400
$EVENTS_ROOT/make_events_statewide.sh 2024
```

| Item | Path / format | Notes |
|------|---------------|--------|
| Lookups | `$CCMMF_MANAGEMENT/plant_traits/planting_lookup.csv`, `harvest_lookup.csv` | CSV; harvest has `destructive` (see [traits/README.md](../../traits/README.md)) |
| Events | `$CCMMF_MANAGEMENT/event_files/{planting,harvest,phenology}_statewide_Y.parquet` (+ `.json`) | [events metadata](../metadata.md) |

For the demo, skip statewide date gap-fill or gap-fill only the subsample. Statewide:

```bash
$PHENOLOGY_ROOT/run_mslsp.sh $YEAR
$PHENOLOGY_ROOT/match_landiq_mslsp.sh $YEAR
$PHENOLOGY_ROOT/run_phenology_date_gapfill.sh $PRIOR_YEAR $TARGET_YEAR
$EVENTS_ROOT/make_events_statewide.sh $YEAR
```

Date gap-fill is required before statewide planting/harvest events.

---

## 2.3 NDTI and tillage events

Tillage is **opt-in** (not in the default `make_events_statewide.sh` run). Timing
comes from NDTI in each fallow window between one season's senescence (`OGMn`)
and the next green-up (`OGI`), using matched phenology from Sec. 2.2.

**A. NDTI extract (demo)**

```bash
TILEWISE_ONE_TILE=$DEMO_TILE $TILLAGE_ROOT/run_ndti.sh 2024
```

| Item | Path / format | Key columns / metadata |
|------|---------------|------------------------|
| Input | HLS reflectance under `data_phen/HLS_data_sort/HLS30/$DEMO_TILE/` | Same imagery tree as phenology |
| Output | `$CCMMF_MANAGEMENT/tillage/ndti_v4.1/year=Y/` | Monthly hive parquet; [ndti_year_metadata.csv](../../tillage/extract/data/ndti_year_metadata.csv) |

**B. Tillage events**

```bash
export CCMMF_MATCHED_DIR=$CCMMF_MANAGEMENT/phenology/matched_landiq_mslsp_v4.1.2/subsample_n400
$EVENTS_ROOT/make_events_statewide.sh 2024 tillage
```

| Item | Path / format | Metadata |
|------|---------------|----------|
| Output | `$CCMMF_MANAGEMENT/event_files/tillage_statewide_Y.parquet` (+ `.json`) | [tillage_statewide_metadata.csv](../../events/data/tillage_statewide_metadata.csv) |

Algorithm detail: [events/README.md](../../events/README.md) (tillage section).

---

## 2.4 Checklist

**Structure checks (not only "job ran"):**

- [ ] `hls_parcel_tile_map_v4.1.rds` and `parcels_10SDH.csv` exist under `$CCMMF_MANAGEMENT`
- [ ] MSLSP NetCDF for `10SDH` under `data_phen/output/`; raw extract hive `phenology/raw_mslsp_v4.1.2/year=2024/` opens
- [ ] Matched parquet has `assigned_by` / `match_outcome`; demo uses `ASSIGN_PARCEL_IDS_FILE`
- [ ] `planting_lookup.csv` and `harvest_lookup.csv` present; harvest has a `destructive` column
- [ ] `planting_statewide_2024.parquet` / `.json`, `harvest_statewide_2024.parquet` / `.json`, `phenology_statewide_2024.parquet` / `.json` under `event_files/`
- [ ] NDTI hive under `tillage/ndti_v4.1/`; `tillage_statewide_2024.parquet` after opt-in run
- [ ] Spot-check: parquet row count > 0; harvest clearing rows use `PFT=woody` + `destructive=TRUE` (no fake PFT)
- [ ] Acceptance: event files are ready for Session 3 combine / SIPNET handoff appendix

**Next:** [Session 3 - Fertilization and irrigation](03-fertilizer-irrigation.md).

**Spine:** [pipeline.md](../pipeline.md).

**Downstream (unofficial):** [SIPNET handoff](sipnet-handoff.md).
