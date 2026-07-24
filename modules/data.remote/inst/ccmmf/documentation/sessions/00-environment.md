# Session 0 - Set up your environment

**Goal:** prepare a cluster account to run the training year pair
(`TARGET_YEAR=2024`, `PRIOR_YEAR=2023`) for the California Cropland Carbon
Monitoring and Modeling Framework (CCMMF).

This session is machine setup only. The end-to-end pipeline map (stages, one
command per stage, links to operator docs) is
[pipeline.md](../pipeline.md).

Log into the head node, clone the repos below, and activate a shared conda
environment that already has the R and Python packages you need.

---

## 0.1 Log in, activate the training environment, and check packages

```bash
# SSH to your cluster head node (site-specific).
# Then activate the shared conda env, for example:
#   conda activate <env_name>
# or:
#   source /path/to/conda.sh && conda activate <env_name>

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
```

Confirm those packages load. If any fail, fix the activate step before continuing.
`CropScapeR` is required for Cropland Data Layer (CDL) download in Session 1.

---

## 0.2 Clone PEcAn (monitoring branch)

CCMMF runnable packages live under PEcAn `modules/data.remote/inst/ccmmf/`.

```bash
# Pick a writable code directory (e.g. $HOME):
mkdir -p "$HOME/src" && cd "$HOME/src"

git clone https://github.com/sarahkanee/pecan.git
cd pecan
git fetch origin feature/ccmmf-statewide-monitoring-inst
git checkout feature/ccmmf-statewide-monitoring-inst

# Scripts live here (until merged to develop):
export CCMMF_CODE="$(pwd)/modules/data.remote/inst/ccmmf"
ls "$CCMMF_CODE"
# landiq-gapfill  phenology  events  hls  tillage  traits  ...
```

After merge into `PecanProject/pecan`, clone upstream `develop` and use the same
`inst/ccmmf` path.

### Also clone cadwr-landuse (Session 1 Python)

Geometry harmonization for LandIQ (statewide crop mapping) is a separate
repository. Use **pixi** inside that clone for the Python stack; the shared
conda env covers the R side in PEcAn.

```bash
cd "$HOME/src"
git clone https://github.com/ccmmf/cadwr-landuse.git
cd cadwr-landuse
# Default branch (main) auto-discovers LandIQ years, including 2024+.
# Pixi: if not already installed - https://pixi.prefix.dev/
export PATH="$HOME/.pixi/bin:$PATH"
```

---

## 0.3 Create a data root

Keep code clones under `$HOME/src` and put large data under a writable root
(default `$HOME/ccmmf`). Pipeline *outputs* land under `management/` inside that
root; runnable code stays in `$CCMMF_CODE`.

```bash
export CCMMF_ROOT="${CCMMF_ROOT:-$HOME/ccmmf}"
mkdir -p "$CCMMF_ROOT"/{data_raw/cadwr_land_use/landiq_shapefiles,data_phen/output,data_phen/HLS_data_sort/HLS30,CDL_data,LandIQ-harmonized-v4.1,LandIQ-harmonized-v4.1.2}

export CCMMF_MANAGEMENT="${CCMMF_MANAGEMENT:-$CCMMF_ROOT/management}"
mkdir -p "$CCMMF_MANAGEMENT"/{phenology,tillage,event_files,plant_traits}
```

**Layout:**

```text
$CCMMF_ROOT/
  data_raw/cadwr_land_use/landiq_shapefiles/   # LandIQ shapefiles by year
  LandIQ-harmonized-v4.1/                     # harmonized geometry + crops
  LandIQ-harmonized-v4.1.2/                   # gap-filled crop product
  data_phen/HLS_data_sort/HLS30/              # HLS reflectance (phenology layout)
  data_phen/output/                           # MSLSP_*.nc per tile
  CDL_data/                                   # cdl_YYYY.tif
  management/                                 # data hub for pipeline outputs
    phenology/raw_mslsp_v4.1.2/
    phenology/matched_landiq_mslsp_v4.1.2/
    tillage/ndti_v4.1/
    event_files/
    plant_traits/
```

`setup_env.sh` (Sec. 0.4) sets package roots such as `LANDIQ_GAPFILL_ROOT` from
`CCMMF_CODE`.

---

## 0.4 Environment file (required)

`setup_env.sh` lives in your PEcAn clone. It uses the paths from Sec. 0.2-0.3
and defaults to **`PRIOR_YEAR=2023`**, **`TARGET_YEAR=2024`**, data root
`$HOME/ccmmf`. With `CCMMF_CODE` set, source it once per shell:

```bash
source "$CCMMF_CODE/documentation/setup_env.sh"
```

That keeps years and package roots (`PHENOLOGY_ROOT`, `TILLAGE_ROOT`,
`EVENTS_ROOT`, …) consistent for later sessions.

For a later year pair, or if you change directory layout, set years and/or paths
**before** sourcing:

```bash
export PRIOR_YEAR=2024
export TARGET_YEAR=2025
# export CCMMF_ROOT=...   # only if not using $HOME/ccmmf
source "$CCMMF_CODE/documentation/setup_env.sh"
```

After Session 1 gap-fill, point everything downstream at the filled product
(also shown in [Session 1](01-landiq.md) / [pipeline.md](../pipeline.md)):

```bash
export CCMMF_LANDIQ_V4=$CCMMF_LANDIQ_GAPFILL_PRODUCT
```

---

## 0.5 Checklist

- [ ] Logged into cluster head node; shared conda env active
- [ ] Confirmed all libraries load
- [ ] Cloned PEcAn monitoring branch; `CCMMF_CODE` points at `inst/ccmmf`
- [ ] Cloned `cadwr-landuse` on `main`; pixi on `PATH` if needed
- [ ] Created `$CCMMF_ROOT` data layout on writable disk
- [ ] Sourced `setup_env.sh` from the clone

**Next:** [Session 1 - LandIQ](01-landiq.md).

**Spine:** [pipeline.md](../pipeline.md).
