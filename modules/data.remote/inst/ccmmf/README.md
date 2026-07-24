# The California Cropland Monitoring and Modeling Framework (CCMMF)

This directory (`modules/data.remote/inst/ccmmf`) is the **monitoring pipeline**
for CCMMF within [PEcAn](https://pecanproject.github.io): the code and operator
documentation that turn public cropland maps and satellite remote sensing into
field-level **management events** (management inputs) for statewide ecosystem
modeling.

CCMMF estimates carbon stocks and greenhouse-gas fluxes on California cropland.
Models such as SIPNET need consistent, parcel-scale records of what was grown and
how it was managed. This tree produces those **events** from LandIQ crop maps,
HLS phenology and tillage indices, crop trait lookups, and linked statewide
workflows for fertilization, organic amendments, and irrigation.

To run this pipeline, follow [documentation/pipeline.md](documentation/pipeline.md).

**Management events covered here**

| Event | Role in the model |
|-------|-------------------|
| Crop identity | Crop / PFT on each parcel-season (defines the field-season for events) |
| Planting | Crop start / initialization |
| Harvest | Biomass removal |
| Phenology | Leaf-on / leaf-off timing |
| Tillage | Soil and residue disturbance |
| N fertilization | Synthetic nitrogen applications *(parallel workflow)* |
| Organic amendments | Manure, compost, biochar, and similar *(parallel workflow)* |
| Irrigation | Water applications *(parallel workflow)* |

## Components in this branch

| Path | Role |
|------|------|
| `landiq-gapfill/` | Fill missing `CLASS` / `SUBCLASS` / `ADOY`; write gap-filled LandIQ product |
| `hls/` | Shared tilewise helpers; build parcel-to-HLS tile map |
| `phenology/` | MSLSP extract, LandIQ-MSLSP match, date gap-fill |
| `traits/` | Planting / harvest trait lookups |
| `tillage/` | NDTI extract, tillage events |
| `events/` | Statewide planting, harvest, phenology, tillage event files |

## Parallel tracks

These products share LandIQ parcel identity with the pipeline above but are run
elsewhere:

| Product | Where | Docs |
|---------|--------|------|
| N fertilization + organic (NCC) amendments | PEcAn `data.land` tables + statewide workflows | [#4002](https://github.com/PecanProject/pecan/pull/4002), [#4003](https://github.com/PecanProject/pecan/pull/4003) |
| Irrigation | `workflows/irrigation-statewide` |

Shared R helpers under a component's `R/` (or `scripts/R/`) are `source()`'d by that
component's scripts. They are not yet part of the installed `PEcAn.data.remote` API.

## External dependencies

| Step | Where |
|------|--------|
| Parcel geometry harmonization | [ccmmf/cadwr-landuse](https://github.com/ccmmf/cadwr-landuse) (Python + pixi) |
| HLS / MSLSP NetCDF production | [HLS_Phenology](https://github.com/mrinareddy/HLS_Phenology) |
