# The California Cropland Carbon Monitoring and Modeling Framework (CCMMF)

Scripts and documentation for California cropland monitoring in PEcAn. This tree
turns statewide crop maps and satellite phenology into field-level management
inputs for ecosystem modeling: crop identity, planting / harvest / phenology,
tillage, plus links to irrigation, N fertilization, and non-crop C (organic)
amendments.

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

These layers share LandIQ parcel identity with the pipeline above but are run
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

To run the pipeline, follow the steps in [documentation/pipeline.md](documentation/pipeline.md).
