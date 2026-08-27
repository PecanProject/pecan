# Session 0 - Setup

**What this session is for.** Later sessions assume a working software stack, the pipeline code on disk, and a data workspace with known path names. This session gets that ready, including what to run every time you open a new shell.

---

## 0.1 Environment (once)

Use conda env `pecan-all-1.15` for all sessions. Activate it and confirm the packages later sessions need. If either check fails, stop and fix the environment before continuing.

```bash
conda activate <ENV_PATH_OR_NAME>   # pecan-all-1.15

Rscript - <<'RS'
stopifnot(
  requireNamespace("arrow"),
  requireNamespace("dplyr"),
  requireNamespace("data.table"),
  requireNamespace("sf"),
  requireNamespace("terra"),
  requireNamespace("exactextractr"),
  requireNamespace("readr"),
  requireNamespace("stringr"),
  requireNamespace("lubridate"),
  requireNamespace("jsonlite")
)
d <- terra::gdal(drivers = TRUE)$name
if (!any(d %in% c("netCDF", "HDF5"))) {
  stop("terra GDAL is missing netCDF/HDF5. conda install -c conda-forge libgdal-hdf5 libgdal-netcdf")
}
RS

python - <<'PY'
import dask
import fiona
import geopandas
import numpy
import pandas
import pyarrow
import shapely
import tqdm
PY
```

---



## 0.2 Clone (once)

Set `$CCMMF_BASE` to the directory that will hold your clones (`$CCMMF_BASE/src`) and data (`$CCMMF_BASE/ccmmf`).

You also need two git repos: PEcAn and [cadwr-landuse](https://github.com/ccmmf/cadwr-landuse). Later sessions assume those repos live at `$CCMMF_BASE/src/pecan` and `$CCMMF_BASE/src/cadwr-landuse`.

**New clones**

```bash
export CCMMF_BASE=/path/to/workdir

mkdir -p "$CCMMF_BASE/src"
cd "$CCMMF_BASE/src"

# pecan -- develop
git clone https://github.com/PecanProject/pecan.git
cd pecan
git checkout develop

# cadwr-landuse -- main
cd "$CCMMF_BASE/src"
git clone https://github.com/ccmmf/cadwr-landuse.git
cd cadwr-landuse
git checkout main
```

**Already cloned elsewhere.**

```bash
export CCMMF_BASE=/path/to/workdir
mkdir -p "$CCMMF_BASE/src"

# ln -s <existing clone> <tutorial path>
ln -s /actual/path/to/pecan          "$CCMMF_BASE/src/pecan"
ln -s /actual/path/to/cadwr-landuse  "$CCMMF_BASE/src/cadwr-landuse"

ls -ld "$CCMMF_BASE/src/pecan" "$CCMMF_BASE/src/cadwr-landuse"
```

---



## 0.3 Every new shell

Activate the conda env from 0.1, set the same `$CCMMF_BASE` as in 0.2, pull the repo you need, and source `setup_env`.

```bash
conda activate <ENV_PATH_OR_NAME>   # same env as 0.1

export CCMMF_BASE=/path/to/workdir   # same as 0.2

# Pull only the repo you are using this session:
git -C "$CCMMF_BASE/src/pecan" pull origin develop
git -C "$CCMMF_BASE/src/cadwr-landuse" pull origin main

# Optional overrides (only if you do not want the BASE defaults):
# export CCMMF_ROOT=/path/to/data
# export PRIOR_YEAR=2023 TARGET_YEAR=2024

source "$CCMMF_BASE/src/pecan/modules/data.remote/inst/ccmmf/documentation/setup_env.sh"
# Also writes workflows/irrigation-statewide/config_paths.yml (irrigation preprocess + tar_make).
```

Heavy steps (Earthdata download, MSLSP, NDTI extract) belong on a compute node. Product scripts have no scheduler headers. Submit with `$CCMMF_SUBMIT` (set by `setup_env.sh`): Slurm (`sbatch`) if present, else Grid Engine (`qsub` when `$SGE_ROOT` is set).

`$CCMMF_SUBMIT` defaults are **4 CPUs, 16G, 12h**. Those are starting guesses -- we are still testing what each step needs. Raise `-c` / `-m` / `-t` if a job is killed; do not start from 16 cores / 64G.

```bash
"$CCMMF_SUBMIT" -n hls-earthdata -c 4 -m 16G -t 48:00:00 -- \
  "$CCMMF_CODE/hls/download_hls_earthdata.sh"
```

Site flags (account, queue, partition, buyin) go in `CCMMF_SUBMIT_EXTRA`. Detail: [submit_job.sh](../submit_job.sh).

---



## 0.4 Workspace (once)

`setup_env` only stored the path strings. Create these folders once. Later sessions use the vars and assume the tree exists.

```text
$CCMMF_ROOT/
  LandIQ/
    raw/                              # LANDIQ_RAW
    work/                             # CADWR_WORK_DIR
      03-final/                       # LANDIQ_HARMONIZED
    gapfilled/                        # LANDIQ_GAPFILLED
  HLS/                                # HLS_ROOT -- imagery + MSLSP + flat prep files
    imagery/                          # HLS_IMAGERY_ROOT
    MSLSP/                            # MSLSP_NETCDF_ROOT -- tile NetCDF; MSLSP_EXTRACT_ROOT under here
  CDL/                                # CDL_DIR
  climate/
    CHIRPS/                           # CHIRPS_DIR
    CIMIS/                            # CIMIS_DIR
  soils/
    SSURGO/                           # SSURGO_DIR
  lookups/
    plant_traits/                     # PLANT_TRAITS_DIR
    fertilization/                    # FERTILIZATION_LOOKUPS
  products/
    inventory/                        # PRODUCTS_INVENTORY
      phenology/                      
      tillage/
      fertilization/
      irrigation/
      event_files/
```

Create the dirs:

```bash
mkdir -p "$LANDIQ_ROOT"/{raw,gapfilled}
mkdir -p "$CADWR_WORK_DIR" "$LANDIQ_HARMONIZED"   # 03-final; S3 skip or cadwr both land here
mkdir -p "$HLS_ROOT"/{imagery,MSLSP}
mkdir -p "$CDL_DIR"
mkdir -p "$CLIMATE_ROOT"/{CHIRPS,CIMIS}
mkdir -p "$SOILS_ROOT"/SSURGO
mkdir -p "$LOOKUPS_ROOT"/{plant_traits,fertilization}
mkdir -p "$PRODUCTS_INVENTORY"/{phenology,tillage,fertilization,irrigation,event_files}
```

---



## 0.5 Confirm setup

Confirm the code and data roots are real on disk.

```bash
ls "$CCMMF_CODE"   # pipeline scripts and documentation
# documentation  events  hls  landiq-gapfill  phenology  tillage  ...

ls "$CCMMF_ROOT"   # data workspace
# CDL  HLS  LandIQ  climate  lookups  products  soils
```

If you do not see the directories you made, go back and fix the section above before Session 1.

---



## 0.6 NASA Earthdata

Create an Earthdata Login account and store credentials in `~/.netrc` for HLS
downloads in [Session 2](02-phenology.md) (Sec. 2.1).

1. Create a free account at [https://urs.earthdata.nasa.gov/](https://urs.earthdata.nasa.gov/)
2. Store credentials in `~/.netrc`:

```bash
# Replace USERNAME and PASSWORD with your Earthdata Login values
echo "machine urs.earthdata.nasa.gov login USERNAME password PASSWORD" > ~/.netrc
chmod 0600 ~/.netrc
```

**Next:** [Session 1 - LandIQ crop identity](01-landiq.md).

**Spine:** [tree README](../../README.md).