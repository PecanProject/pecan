# CCMMF (`inst/ccmmf`)

Scripts and training docs for **California cropland monitoring** in PEcAn: turn
public statewide crop maps and satellite phenology into field-level management
inputs (and related products) for ecosystem modeling.

This folder is the portable home for that workflow. It will grow beyond phenology
alone (crop gap-fill, tillage, irrigation hooks, lookups, etc.).

**Start here:** [documentation/pipeline.md](documentation/pipeline.md)  
**Then:** [documentation/sessions/](documentation/sessions/) (Session 0 = machine setup)

## What is in this folder

| Path | What it is |
|------|------------|
| `documentation/` | How to run the year-pair update: `pipeline.md` (spine) + `sessions/` (walkthroughs) + `ccmmf_env.example.sh` (env template) |
| `landiq-gapfill/` | Fills missing crop class/subclass and peak-greenness day (`ADOY`) on the harmonized LandIQ table; writes the gap-filled product used downstream |
| `mslsp-extract/` | Extracts MSLSP (multi-sensor land surface phenology) metrics onto LandIQ parcels from NetCDF tile products |
| `ndti-extract/` | Extracts NDTI (tillage-related index) onto parcels from HLS imagery |
| `hls/` | Shared helpers for HLS-based extracts, plus building the parcel-to-HLS-tile map |
| `phenology/` | Matches LandIQ crop seasons to MSLSP cycles; optional date gap-fill on matched rows |
| `traits/` | Builds planting/harvest trait lookup tables (LAI and C/N pools for planting events) |
| `events/` | Writes statewide phenology, planting, harvest, and (opt-in) tillage event files |
| `tillage/` | Helpers that turn NDTI time series into tillage timing / intensity metrics |
| `LandIQ_cropCode_lookup_table.csv` | LandIQ CLASS/SUBCLASS metadata, agricultural flag, and plant-functional-type labels |

Shared R helpers for a workflow live under that workflow's `scripts/_lib/` (local
library code sourced by the scripts - not a separate R package yet).

## First-time setup

1. Clone PEcAn and check out the monitoring branch that contains this tree.
2. From **this** directory (`inst/ccmmf`), note its absolute path - that is
   `CCMMF_CODE`.
3. Create a writable data root (call it `CCMMF_ROOT`) for LandIQ, CDL, HLS, and
   outputs. It does not have to live inside PEcAn.
4. Copy the env template next to your data (or into your project dir), edit paths,
   and `source` it:

```bash
# Example if your shell is already in modules/data.remote/inst/ccmmf:
export CCMMF_CODE="$(pwd)"
export CCMMF_ROOT="${CCMMF_ROOT:-$HOME/ccmmf}"   # change if you prefer another data root
mkdir -p "$CCMMF_ROOT"
cp "$CCMMF_CODE/documentation/ccmmf_env.example.sh" "$CCMMF_ROOT/ccmmf_env.sh"
# Edit CCMMF_ROOT, CCMMF_CODE, years, and product paths in that file, then:
source "$CCMMF_ROOT/ccmmf_env.sh"
```

5. Follow [documentation/pipeline.md](documentation/pipeline.md) starting at
   Session 0.

Geometry harmonization (Python) is a separate repo:
[ccmmf/cadwr-landuse](https://github.com/ccmmf/cadwr-landuse). Upstream HLS/MSLSP
NetCDF production uses [HLS_Phenology](https://github.com/mrinareddy/HLS_Phenology).

If extract scripts cannot find shared HLS helpers automatically, set
`HLS_SHARED_LIB` to `$CCMMF_CODE/hls/_lib` in your env file.
