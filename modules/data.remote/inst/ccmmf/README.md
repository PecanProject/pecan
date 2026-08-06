# The California Cropland Monitoring and Modeling Framework (CCMMF)

This directory (`modules/data.remote/inst/ccmmf`) is the **Management Tracking**
monitoring pipeline for CCMMF within [PEcAn](https://pecanproject.github.io):
code and documentation that turn public cropland maps and satellite remote
sensing into field-level **management events** for the MAGIC annual inventory
and scenario projections (SIPNET).

CCMMF estimates carbon stocks and greenhouse-gas fluxes on California cropland.
Models such as SIPNET need consistent, parcel-scale records of what was grown and
how it was managed. This tree produces those **events** from LandIQ crop maps,
HLS phenology and tillage indices, crop trait lookups (CSV), and linked statewide
workflows for fertilization, organic amendments, and irrigation.

**Start here:** [documentation/pipeline.md](documentation/pipeline.md)
(product map, annual update SOP, QC). Session index:
[documentation/README.md](documentation/README.md).

**Management events covered here**

| Event | Role in the model |
|-------|-------------------|
| Crop identity | Crop / PFT on each parcel-season (defines the field-season for events) |
| Planting | Crop start / pool initialization |
| Harvest | Biomass removal |
| Phenology | Leaf-on / leaf-off timing |
| Tillage | Soil and residue disturbance |
| N fertilization | Synthetic nitrogen applications *(parallel workflow; Session 3)* |
| Organic amendments | Manure, compost, biochar, and similar *(parallel workflow; Session 3)* |
| Irrigation | Water applications *(parallel workflow; Session 3)* |

## Components in this branch

| Path | Role |
|------|------|
| `landiq-gapfill/` | Fill missing `CLASS` / `SUBCLASS` / `ADOY`; write gap-filled LandIQ product |
| `hls/` | Shared tilewise helpers; build parcel-to-HLS tile map |
| `phenology/` | MSLSP extract, LandIQ-MSLSP match, date gap-fill |
| `traits/` | Planting / harvest trait lookups (CSV) |
| `tillage/` | NDTI extract, tillage events |
| `events/` | Statewide planting, harvest, phenology, tillage event files |
| `documentation/` | Pipeline map, Sessions 0-3, SIPNET handoff appendix |

## Parallel tracks

These products share LandIQ parcel identity with the pipeline above but are run
elsewhere:

| Product | Where | Docs |
|---------|--------|------|
| N fertilization + organic (NCC) amendments | `PEcAn.data.land` + statewide workflows | [Session 3](documentation/sessions/03-fertilizer-irrigation.md) |
| Irrigation | `workflows/irrigation-statewide` | [Session 3](documentation/sessions/03-fertilizer-irrigation.md) |
| SIPNET handoff (unofficial) | `data.land` / `PEcAn.SIPNET` / preprocess workflows | [sipnet-handoff.md](documentation/sessions/sipnet-handoff.md) |

Shared R helpers under a component's `R/` (or `scripts/R/`) are `source()`'d by that
component's scripts. They are not yet part of the installed `PEcAn.data.remote` API.

## External dependencies

| Step | Where |
|------|--------|
| Parcel geometry harmonization | [ccmmf/cadwr-landuse](https://github.com/ccmmf/cadwr-landuse) (Python + pixi) |
| HLS / MSLSP NetCDF production | [HLS_Phenology](https://github.com/mrinareddy/HLS_Phenology) |
