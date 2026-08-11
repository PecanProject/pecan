# Session 0 - Setup

**What this session is for.** Before downloading LandIQ or running HLS extracts, you need a working software stack, the pipeline code on disk, a data workspace with known path names, and (for Session 2) a NASA Earthdata login. This session gets that environment ready; it does not build inventory products yet.

**Prerequisite:** install `pecan-all-1.14` before this walkthrough if you do not already have it.

**Where to go deeper:** path names in [setup_env.sh](../setup_env.sh); product overview in [tree README](../../README.md).

---

## 0.1 Environment

Log into the head node, activate `pecan-all-1.14`, and confirm the R and Python package checks pass. Examples below use `$HOME` -- change them if the environment lives somewhere else.

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

Confirm both checks pass. If either fails, stop and fix the environment before continuing.

---



## 0.2 Repos

Pipeline scripts live under `modules/data.remote/inst/ccmmf` in the PEcAn clone (`$CCMMF_CODE`). Geometry for Session 1 uses `cadwr-landuse`.

Clone each repo if it is not on disk yet; if it already exists, skip clone and only `cd` + `pull`.

```bash
mkdir -p "$HOME/src"
cd "$HOME/src"

# pecan -- monitoring branch (training docs + inst/ccmmf)
git clone https://github.com/sarahkanee/pecan.git   # skip if already cloned
cd pecan
git checkout feature/ccmmf-statewide-monitoring-inst
git pull origin feature/ccmmf-statewide-monitoring-inst

export CCMMF_CODE="$(pwd)/modules/data.remote/inst/ccmmf"

# cadwr-landuse -- main
cd "$HOME/src"
git clone https://github.com/ccmmf/cadwr-landuse.git   # skip if already cloned
cd cadwr-landuse
git checkout main
git pull origin main
```

---



## 0.3 `setup_env.sh`

Create a data workspace and source `setup_env` once per shell. That sets the inventory year pair, `$CCMMF_ROOT` defaults, and component roots under `$CCMMF_CODE`.

```bash
# Optional overrides before sourcing (defaults: 2023/2024, $HOME/ccmmf):
# export PRIOR_YEAR=2023 TARGET_YEAR=2024
# export CCMMF_ROOT=/path/to/data
source "$CCMMF_CODE/documentation/setup_env.sh"
```

---



## 0.4 Data layout

<a id="data-layout"></a>

Finished `$CCMMF_ROOT` workspace (defaults from [setup_env.sh](../setup_env.sh)).
`setup_env` exports the path names; create the workspace once here. Later
sessions only refer to these vars (no per-session mkdir).

```text
$CCMMF_ROOT/
  LandIQ/
    raw/                              # LANDIQ_RAW
    work/                             # CADWR_WORK_DIR
      03-final/                       # LANDIQ_HARMONIZED
    gapfilled/                        # LANDIQ_GAPFILLED
  HLS/
    imagery/                          # HLS_IMAGERY_ROOT
    MSLSP/                            # MSLSP_NETCDF_ROOT
  CDL/                                # CDL_DIR
  climate/
    CHIRPS/                           # CHIRPS_DIR
    CIMIS/                            # CIMIS_DIR
  soils/
    SSURGO/                           # SSURGO_DIR
  lookups/
    plant_traits/                     # PLANT_TRAITS_DIR
    fertilization/                    # FERTILIZATION_LOOKUPS (rate tables)
  products/
    inventory/                        # PRODUCTS_INVENTORY
      phenology/                      # MATCHED_DIR default under here
      tillage/
      fertilization/
      irrigation/
      event_files/
      demo/
    projections/                      # PRODUCTS_PROJECTIONS
```

Create the dirs:

```bash
mkdir -p "$LANDIQ_ROOT"/{raw,gapfilled}
mkdir -p "$CADWR_WORK_DIR"   # LANDIQ_HARMONIZED -> $CADWR_WORK_DIR/03-final after cadwr
mkdir -p "$HLS_ROOT"/{imagery,MSLSP}
mkdir -p "$CDL_DIR"
mkdir -p "$CLIMATE_ROOT"/{CHIRPS,CIMIS}
mkdir -p "$SOILS_ROOT"/SSURGO
mkdir -p "$LOOKUPS_ROOT"/{plant_traits,fertilization}
mkdir -p "$PRODUCTS_INVENTORY"/{phenology,tillage,fertilization,irrigation,event_files,demo}
mkdir -p "$PRODUCTS_PROJECTIONS"
```

---



## 0.5 NASA Earthdata

Create an Earthdata Login account and store credentials in `~/.netrc` for HLS downloads (Session 2).

1. Create a free account at [https://urs.earthdata.nasa.gov/](https://urs.earthdata.nasa.gov/)
2. Store credentials in `~/.netrc`:

```bash
# Replace USERNAME and PASSWORD with your Earthdata Login values
echo "machine urs.earthdata.nasa.gov login USERNAME password PASSWORD" > ~/.netrc
chmod 0600 ~/.netrc
```

---



## 0.6 Confirm setup

Confirm the environment and paths are real on disk.

```bash
# Code + data roots (should print non-empty paths)
echo "CCMMF_CODE=$CCMMF_CODE"
echo "CCMMF_ROOT=$CCMMF_ROOT"
echo "YEARS=$PRIOR_YEAR / $TARGET_YEAR"

# Pipeline tree present
ls "$CCMMF_CODE"
# expect: landiq-gapfill  documentation  phenology  events  ...

# Workspace dirs from Sec. 0.4
ls -d "$LANDIQ_RAW" "$LANDIQ_GAPFILLED" "$CADWR_WORK_DIR" "$CDL_DIR" "$HLS_ROOT"

# cadwr clone (adjust path if you put it elsewhere)
ls "$HOME/src/cadwr-landuse/scripts/01-split.py"

# Earthdata creds file exists and is private (Session 2); skip if delaying HLS
ls -l ~/.netrc
```

If any `ls` fails, fix that section above before Session 1.

**Next:** [Session 1 - LandIQ crop identity](01-landiq.md).

**Spine:** [tree README](../../README.md).