# Session 0 - Setup

**Deliverable:** a ready operating environment for the CCMMF Management Tracking
pipeline (inputs to the MAGIC annual inventory).

**Goal:** prepare a machine to run the operational year pair
(`TARGET_YEAR=2024`, `PRIOR_YEAR=2023`) for the California Cropland Monitoring
and Modeling Framework (CCMMF).

**Method / maturity:** environment and repository setup (not a data product).

**Prerequisite:** install `pecan-all-1.12` before this walkthrough if you do
not already have it. This session assumes that environment is already installed.

Paths below use `$HOME` as an example; replace them with writable locations on
your system.

---

## Where you are

Same flow as [pipeline.md](../pipeline.md). This session prepares the machine
before Session 1.

```mermaid
flowchart TB
  subgraph S0["Session 0 - Setup - you are here"]
    ENV["conda + repos\n+ data root + setup_env"]
  end

  subgraph S1["Session 1 - Crop identity"]
    GF["LandIQ + gap-fill"]
  end

  subgraph S2["Session 2 - HLS events"]
    HLS["MSLSP + NDTI events"]
  end

  subgraph S3["Session 3 - Fert + irrigation"]
    FI["N rates + water-balance"]
  end

  ENV --> GF
  GF --> HLS
  GF --> FI
  HLS --> OUT["Management event files"]
  FI --> OUT
```

This session = Session 0 (env, repos, `$CCMMF_ROOT`, Earthdata).

---

## 0.1 Environment

Log into the head node, activate `pecan-all-1.12`, and confirm the R and Python
package checks pass.

```bash
# SSH to your cluster head node.
# Default install path from setup-pecan-env.sh:
#   conda activate "$HOME/.conda/envs/pecan-all"
which conda
conda activate <ENV_PATH_OR_NAME>

which Rscript
Rscript -e 'stopifnot(
  requireNamespace("arrow"),
  requireNamespace("dplyr"),
  requireNamespace("data.table"),
  requireNamespace("sf"),
  requireNamespace("terra"),
  requireNamespace("exactextractr"),
  requireNamespace("readr"),
  requireNamespace("stringr"),
  requireNamespace("lubridate"),
  requireNamespace("jsonlite"),
  requireNamespace("units"),
  requireNamespace("CropScapeR")
)'

python - <<'PY'
import dask
import fiona
import geopandas
import numpy
import pandas
import pyarrow
import shapely
import tqdm
print("Python GIS dependencies: OK")
PY
```

Confirm both checks pass. If either fails, stop and fix the environment before
continuing.

---



## 0.2 Repos

Clone the PEcAn monitoring branch and `cadwr-landuse`. Scripts live under
`modules/data.remote/inst/ccmmf` in the PEcAn clone.

```bash
mkdir -p "$HOME/src" && cd "$HOME/src"

git clone https://github.com/sarahkanee/pecan.git
cd pecan
git fetch origin feature/ccmmf-statewide-monitoring-inst
git checkout feature/ccmmf-statewide-monitoring-inst

# Until merged to develop:
export CCMMF_CODE="$(pwd)/modules/data.remote/inst/ccmmf"
ls "$CCMMF_CODE"
# landiq-gapfill  phenology  events  hls  tillage  traits  ...
```

After merge into `PecanProject/pecan`, clone upstream `develop` and use the same
`inst/ccmmf` path.

Geometry harmonization for LandIQ (Session 1) is a separate repository:

```bash
cd "$HOME/src"
git clone https://github.com/ccmmf/cadwr-landuse.git
```

---

## 0.3 `setup_env.sh`

Source once per shell. This sets years, `$CCMMF_ROOT`, input/lookup/product
roots, and component roots (`PHENOLOGY_ROOT`, `TILLAGE_ROOT`, ...).

```bash
# Optional overrides before sourcing (defaults: 2023/2024, $HOME/ccmmf):
# export PRIOR_YEAR=2023 TARGET_YEAR=2024
# export CCMMF_ROOT=/path/to/data
source "$CCMMF_CODE/documentation/setup_env.sh"
```

---

## 0.4 Data directories

`setup_env` only exports paths; create the directories once:

```bash
mkdir -p "$LANDIQ_ROOT"/{raw,harmonized,gapfilled}
mkdir -p "$HLS_ROOT"/{imagery,MSLSP}
mkdir -p "$CDL_DIR"
mkdir -p "$CLIMATE_ROOT"/{CHIRPS,CIMIS}
mkdir -p "$SOILS_ROOT"/SSURGO
mkdir -p "$LOOKUPS_ROOT"/{plant_traits,fertilization}
mkdir -p "$PRODUCTS_INVENTORY"/{phenology,tillage,fertilization,irrigation,event_files}
mkdir -p "$PRODUCTS_PROJECTIONS"
```

**Layout** (three roles: inputs, lookups, products):

```text
$CCMMF_ROOT/
  LandIQ/                                     # crop inventory inputs
    raw/                                      # annual shapefiles as downloaded
    harmonized/                               # geometry + crops (pre-gap-fill)
    gapfilled/                                # gap-filled crops product
  HLS/
    imagery/                                  # HLS GeoTIFF
    MSLSP/                                    # HLS Phenology Product (MSLSP NetCDF)
  CDL/                                        # USDA Cropland Data Layer
  climate/                                    # weather / ET (irrigation)
    CHIRPS/
    CIMIS/
  soils/
    SSURGO/
  lookups/                                    # plant_traits, fertilization rates
    plant_traits/
    fertilization/                            # N / organic rate tables
  products/
    inventory/                                # Management Tracking ($PRODUCTS_INVENTORY)
      phenology/
      tillage/
      fertilization/                          # fert/NCC event outputs
      irrigation/                             # water-balance + irrig events
      event_files/                            # planting/harvest/phenology/tillage
    projections/                              # scenarios / model outputs
```

| Item | Path | Notes |
|------|------|--------|
| Code | `$CCMMF_CODE` | `inst/ccmmf` scripts |
| Data | `$CCMMF_ROOT` | Inputs + lookups + products |
| Inventory | `$PRODUCTS_INVENTORY` | Management Tracking outputs |
| Projections | `$PRODUCTS_PROJECTIONS` | Scenario / model outputs |

---



## 0.5 NASA Earthdata

Create an Earthdata Login account and store credentials in `~/.netrc` for HLS
downloads (Session 2).

1. Create a free account at [https://urs.earthdata.nasa.gov/](https://urs.earthdata.nasa.gov/)
2. Store credentials in `~/.netrc`:

```bash
# Replace USERNAME and PASSWORD with your Earthdata Login values
echo "machine urs.earthdata.nasa.gov login USERNAME password PASSWORD" > ~/.netrc
chmod 0600 ~/.netrc
```

---



## 0.6 Checklist

- [ ] `pecan-all-1.12` already installed
- [ ] Activated conda env; R and Python checks pass
- [ ] Cloned PEcAn monitoring branch; `CCMMF_CODE` points at `inst/ccmmf`
- [ ] Cloned `cadwr-landuse` on `main`
- [ ] Sourced `setup_env.sh`; `$CCMMF_ROOT`, `$PRODUCTS_INVENTORY`, `$PHENOLOGY_ROOT`, ... set
- [ ] Data dirs created under `$CCMMF_ROOT` (inputs / lookups / products)
- [ ] Earthdata account + `~/.netrc` ready (for Session 2)

**Next:** [Session 1 - LandIQ crop identity](01-landiq.md).

**Spine:** [pipeline.md](../pipeline.md).