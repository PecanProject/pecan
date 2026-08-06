# CCMMF monitoring pipeline

California cropland monitoring for PEcAn: turn public crop maps and satellite
data into field-level **management events** (management inputs) for ecosystem
models.

For each new LandIQ release you process a **year pair**: the new year
(`TARGET_YEAR`, training default **2024**) and the prior year (`PRIOR_YEAR`,
**2023**). Gap-fill, extracts, matching, and events are refreshed for both.

## Demo (one tile) vs statewide

| Path | What |
|------|------|
| **Demo** | HLS tile `10SDH`; match/events limited with `ASSIGN_PARCEL_IDS_FILE` |
| **Statewide** | All tiles / full match; same steps without the tile / parcel-list limits |

Walk through Sessions 0-3 in order. Each session covers how to build inputs, run,
and verify outputs for that stage.

## What you build

| Management event | Plain language | Main source |
|------------------|----------------|-------------|
| Crop identity | Which crop is on each field each season | LandIQ (+ CDL gap-fill) |
| Planting | When the model starts the crop | HLS phenology (green-up) + traits |
| Harvest | When biomass is removed | HLS phenology (senescence) + traits |
| Phenology | Leaf-on / leaf-off timing | HLS Multisource Land Surface Phenology (MSLSP) |
| Tillage | Soil/residue disturbance in fallow windows | Normalized Difference Tillage Index (NDTI) |
| N fertilization | Nitrogen applied to the crop | Crop guidelines / lookups (not satellites) |
| Organic amendments | Manure, compost, biochar, and similar | Material guidelines / lookups (not satellites) |
| Irrigation | Water applied over the season | Precip (CHIRPS), reference ET (CIMIS), soil AWC (SSURGO) |

## Data sources and accounts

| Session | Data | Account needed? |
|---------|------|-----------------|
| 1 | LandIQ (CNRA) | No (public download) |
| 1 | CDL (CropScape / `CropScapeR`) | No API key for the default statewide download |
| 2 | HLS / MSLSP / NDTI ([HLS_Phenology](https://github.com/mrinareddy/HLS_Phenology)) | Yes: [NASA Earthdata Login](https://urs.earthdata.nasa.gov/) |
| 3 | Fertilization lookups; CHIRPS, CIMIS, SSURGO | Usually public / preprocessed extracts; follow Session 3 if a portal account is required |

Create the Earthdata account in [Session 0](sessions/00-setup.md) before HLS builds.

## Run order by session

Follow each session walkthrough; open the linked README for flags and schemas.
Machine setup (clone repos, source `setup_env.sh`): [Session 0](sessions/00-setup.md).

### Session 1 - Crop identity (LandIQ)

Why: models need a stable field map and a crop label for every parcel-year.

| Step | Output | Detail |
|------|--------|--------|
| Download LandIQ `TARGET_YEAR` | Shapefile under `landiq_shapefiles/` | [Session 1](sessions/01-landiq.md) |
| Harmonize geometry | `$CCMMF_LANDIQ_V4` | [cadwr-landuse](https://github.com/ccmmf/cadwr-landuse) |
| Gap-fill `PRIOR,TARGET` | `$CCMMF_LANDIQ_GAPFILL_PRODUCT` | [landiq-gapfill/README.md](../landiq-gapfill/README.md) |

```bash
$LANDIQ_GAPFILL_ROOT/run_gapfill.sh ${PRIOR_YEAR},${TARGET_YEAR}
export CCMMF_LANDIQ_V4=$CCMMF_LANDIQ_GAPFILL_PRODUCT
```

### Session 2 - HLS events (phenology and tillage)

Why: green-up and senescence dates drive planting, harvest, and phenology
events; NDTI in fallow windows drives tillage.

| Step | Output | Detail |
|------|--------|--------|
| Parcel-tile map (once) | `hls_parcel_tile_map_v4.1.rds` | [hls/README.md](../hls/README.md) |
| MSLSP extract | `phenology/raw_mslsp_v4.1.2/year=Y/` | [phenology/extract/README.md](../phenology/extract/README.md) |
| Match LandIQ seasons to MSLSP cycles | `assigned_year=Y.parquet` | [phenology/match/README.md](../phenology/match/README.md) |
| Trait lookups (one-time) | `plant_traits/planting_lookup.csv`, `harvest_lookup.csv` | [traits/README.md](../traits/README.md) |
| Date gap-fill | `gapfill_dates/` overlays | [phenology/gapfill/README.md](../phenology/gapfill/README.md) |
| Planting + harvest + phenology events | `event_files/*_statewide_Y*` | [events/README.md](../events/README.md) |
| NDTI extract | `tillage/ndti_v4.1/year=Y/` | [tillage/extract/README.md](../tillage/extract/README.md) |
| Tillage events (opt-in) | `event_files/tillage_statewide_Y*` | [events/README.md](../events/README.md) |

Walkthrough: [sessions/02-phenology.md](sessions/02-phenology.md)

Run MSLSP, match, date gap-fill, and events for **both** `PRIOR_YEAR` and
`TARGET_YEAR`. Date gap-fill is required before statewide planting/harvest
events. Tillage is a separate opt-in call.

```bash
# Demo (one tile):
TILEWISE_ONE_TILE=10SDH $PHENOLOGY_ROOT/run_mslsp.sh $YEAR
ASSIGN_PARCEL_IDS_FILE=$CCMMF_MANAGEMENT/demo/parcels_10SDH.csv \
  $PHENOLOGY_ROOT/match_landiq_mslsp.sh $YEAR
export CCMMF_MATCHED_DIR=$CCMMF_MANAGEMENT/phenology/matched_landiq_mslsp_v4.1.2/subsample_n400
$EVENTS_ROOT/make_events_statewide.sh $YEAR

TILEWISE_ONE_TILE=10SDH $TILLAGE_ROOT/run_ndti.sh $YEAR
$EVENTS_ROOT/make_events_statewide.sh $YEAR tillage

# Statewide:
$PHENOLOGY_ROOT/run_mslsp.sh $YEAR
$PHENOLOGY_ROOT/match_landiq_mslsp.sh $YEAR
$PHENOLOGY_ROOT/run_phenology_date_gapfill.sh $PRIOR_YEAR $TARGET_YEAR
$EVENTS_ROOT/make_events_statewide.sh $YEAR
$TILLAGE_ROOT/run_ndti.sh $YEAR
$EVENTS_ROOT/make_events_statewide.sh $YEAR tillage
```

### Session 3 - Fertilization and irrigation

Why: fertilizer and organic amendments are not visible from satellites, so they
use rate lookups and separate statewide workflows. Irrigation is estimated with
a water-balance model (plus LandIQ irrigation type), not from the phenology
event builder.

| Step | Output | Detail |
|------|--------|--------|
| N fertilization + organic amendments | Parcel N / amendment event parquets | [Session 3](sessions/03-fertilizer-irrigation.md); [#4002](https://github.com/PecanProject/pecan/pull/4002), [#4003](https://github.com/PecanProject/pecan/pull/4003) |
| Irrigation events (CHIRPS + CIMIS + SSURGO) | Irrigation event files / parquet | [Session 3](sessions/03-fertilizer-irrigation.md); same parcel list as Session 2 |

## Picture of the flow

```mermaid
flowchart TB
  subgraph S1["Session 1 - Crop identity"]
    DWR["LandIQ shapefile"] --> CADWR["Harmonize geometry\ncadwr-landuse"]
    CADWR --> GF["Gap-fill crops + ADOY\nlandiq-gapfill"]
  end

  subgraph S2["Session 2 - HLS events"]
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
    FERT["N fert + organic\namendments\nPRs 4002 / 4003"]
    IRR["Irrigation events\nwater-balance workflow"]
  end

  EV1 --> OUT["Management event files\nfor models"]
  EV2 --> OUT
  FERT --> OUT
  IRR --> OUT
```

Planting, harvest, phenology, and tillage events are built in this tree
(`events/`). N fertilization, organic amendments, and irrigation use **parallel
workflows** that share the same LandIQ parcels (Session 3).

## Checklist (training: 2024 + re-run 2023)

| Session | Done | Checkpoint |
|---------|------|------------|
| 0 | [ ] | Env sourced; repos cloned; `$CCMMF_ROOT` layout ready |
| 1 | [ ] | Built harmonized table; ran gap-fill `2023,2024`; `CCMMF_LANDIQ_V4` -> gap-filled product |
| 2 | [ ] | One-tile MSLSP + match; planting/harvest/phenology events; one-tile NDTI + tillage |
| 3 | [ ] | Fert / organic reviewed; irrigation on demo parcels reviewed or run |

Code layout: [../README.md](../README.md).
Env template: [setup_env.sh](setup_env.sh) (see [Session 0](sessions/00-setup.md)).
Column dictionaries: [metadata.md](metadata.md).
