# The California Cropland Carbon Monitoring and Modeling Framework (CCMMF)

This directory (`modules/data.remote/inst/ccmmf`) is the **monitoring pipeline**
for CCMMF within [PEcAn](https://pecanproject.github.io): the code and operator
documentation that turn public cropland maps and satellite remote sensing into
field-level **management inputs** for statewide ecosystem modeling.

CCMMF estimates carbon stocks and greenhouse-gas fluxes on California cropland.
Models such as SIPNET need consistent, parcel-scale records of what was grown and
how it was managed. This tree produces those records from LandIQ crop maps, HLS
phenology and tillage indices, crop trait lookups, and linked statewide
workflows for fertilization, organic amendments, and irrigation.

**Management inputs covered here**

| Input | Role in the model |
|-------|-------------------|
| Crop identity | Crop / PFT on each parcel-season |
| Planting | Crop start / initialization |
| Harvest | Biomass removal |
| Phenology | Leaf-on / leaf-off timing |
| Tillage | Soil and residue disturbance |
| N fertilization | Synthetic nitrogen applications *(parallel workflow)* |
| Organic amendments | Manure, compost, biochar, and similar *(parallel workflow)* |
| Irrigation | Water applications *(parallel workflow)* |

To run this pipeline, follow [documentation/pipeline.md](documentation/pipeline.md).

## Packages in this tree

| Path | Role |
|------|------|
| `landiq-gapfill/` | Fill missing `CLASS` / `SUBCLASS` / `ADOY`; write gap-filled LandIQ product |
| `hls/` | Shared tilewise helpers; build parcel-to-HLS tile map (once) |
| `phenology/` | MSLSP extract, LandIQ-MSLSP match, date gap-fill (Session 2 track) |
| `tillage/` | NDTI extract; tillage events via `events/` |
| `traits/` | Planting / harvest trait lookups |
| `events/` | Statewide planting, harvest, phenology, tillage event files |
| `landiq-gapfill/data/LandIQ_cropCode_lookup_table.csv` | CLASS/SUBCLASS to PFT / agricultural flag |

## Parallel tracks

These products share LandIQ parcel identity with the pipeline above but are run
elsewhere:

| Product | Where | Docs |
|---------|--------|------|
| N fertilization + organic (NCC) amendments | PEcAn `data.land` tables + statewide workflows | [#4002](https://github.com/PecanProject/pecan/pull/4002), [#4003](https://github.com/PecanProject/pecan/pull/4003); [Session 3](documentation/sessions/03-tillage-fertilizer.md) |
| Irrigation | `workflows/irrigation-statewide` | [Session 4](documentation/sessions/04-irrigation.md) |

Shared R helpers under a package's `R/` (or `scripts/R/`) are `source()`'d by that
package's scripts. They are not yet part of the installed `PEcAn.data.remote` API.

## External dependencies

| Step | Where |
|------|--------|
| Parcel geometry harmonization | [ccmmf/cadwr-landuse](https://github.com/ccmmf/cadwr-landuse) (Python + pixi) |
| HLS / MSLSP NetCDF production | [HLS_Phenology](https://github.com/mrinareddy/HLS_Phenology) |

## First-time setup

1. Clone PEcAn; check out the monitoring branch that contains this tree.
2. Set `CCMMF_CODE` to the absolute path of this directory.
3. Create a writable data root (`CCMMF_ROOT`, e.g. `$HOME/ccmmf`).
4. Source the env template:

```bash
export CCMMF_CODE="$(pwd)"   # if already in this directory
export CCMMF_ROOT="${CCMMF_ROOT:-$HOME/ccmmf}"
source "$CCMMF_CODE/documentation/setup_env.sh"
```

5. Follow [documentation/pipeline.md](documentation/pipeline.md).
