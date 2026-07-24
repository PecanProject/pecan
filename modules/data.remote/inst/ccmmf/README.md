# CCMMF (`inst/ccmmf`)

Scripts and documentation for California cropland monitoring in PEcAn. This tree
turns statewide crop maps and satellite phenology into field-level management
inputs for ecosystem modeling: crop identity, planting / harvest / phenology,
tillage, plus links to irrigation, N fertilization, and non-crop C (organic)
amendments.

**Start here:** [documentation/pipeline.md](documentation/pipeline.md)  
**Machine setup:** [documentation/sessions/00-environment.md](documentation/sessions/00-environment.md)

Column dictionaries for each product live next to that package under
`data/*_metadata.csv` (index: [documentation/metadata.md](documentation/metadata.md)).

## Documentation layers

1. **`documentation/pipeline.md`** - end-to-end map (order, env, checklist, links).
2. **Package `README.md` files** (table below) - how each stage's code works.
3. **`documentation/sessions/`** - training walkthroughs that link to those READMEs.

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

Each product that ships a column dictionary in this tree keeps it under
`data/*_metadata.csv` (see [metadata.md](documentation/metadata.md)).

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
