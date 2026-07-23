# Session 0 - Set up your machine (year pair 2024 + 2023)

**Goal:** one Linux cloud / HPC account ready to run the training year pair
(`TARGET_YEAR=2024`, `PRIOR_YEAR=2023`): LandIQ -> gap-fill -> phenology -> events.

**Next:** [Session 1 - LandIQ](01-landiq.md). **Spine:** [pipeline.md](../pipeline.md).

**Code:** PEcAn `modules/data.remote/inst/ccmmf/` (monitoring branch).

---

## 0.1 What you will install

| Piece | Role |
|-------|------|
| **PEcAn** (this branch) | R workflows: gap-fill, extract, match, events (`inst/ccmmf/`) |
| **cadwr-landuse** | Python / **pixi**: LandIQ geometry harmonization (Session 1) |
| **R >= 4.4** + packages | Gap-fill through events |
| **GDAL / PROJ / GEOS** | Spatial I/O (`sf`, `terra`) |
| **Data root** `$CCMMF_ROOT` | LandIQ, HLS, CDL, products (you choose the path) |
| **[HLS_Phenology](https://github.com/mrinareddy/HLS_Phenology)** (external) | HLS download + MSLSP NetCDF (Session 2) |

Lab `module load` / `qsub -l buyin` are examples only. On your site use system
packages or conda/spack and run `run_*.sh` / `Rscript` (and `pixi run` for
harmonize).

---

## 0.2 Clone PEcAn (monitoring branch)

```bash
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

```bash
git clone https://github.com/ccmmf/cadwr-landuse.git
cd cadwr-landuse
# Until year auto-discover is on main:
git fetch origin && git checkout feature/auto-discover-landiq-years
# Install pixi once: https://pixi.prefix.dev/
export PATH="$HOME/.pixi/bin:$PATH"
```

---

## 0.3 Create a data root (not `/projectnb`)

Pick a writable directory with enough space (statewide HLS + CDL is large):

```bash
export CCMMF_ROOT="$HOME/ccmmf"          # or /data/ccmmf, /scratch/ccmmf, ...
mkdir -p "$CCMMF_ROOT"/{data_raw/cadwr_land_use/landiq_shapefiles,data_phen/output,data_phen/HLS_data_sort/HLS30,CDL_data,LandIQ-harmonized-v4.1,LandIQ-harmonized-v4.1.2}

# Code may live inside PEcAn; management-style layout for outputs:
export CCMMF_MANAGEMENT="${CCMMF_MANAGEMENT:-$CCMMF_ROOT/management}"
mkdir -p "$CCMMF_MANAGEMENT"/{phenology,tillage,event_files,plant_traits}
```

**Layout users should mirror:**

```text
$CCMMF_ROOT/
  data_raw/cadwr_land_use/landiq_shapefiles/   # LandIQ shapefiles by year
  LandIQ-harmonized-v4.1/                     # harmonized geometry + crops
  LandIQ-harmonized-v4.1.2/                   # gap-filled product (downstream)
  data_phen/HLS_data_sort/HLS30/              # HLS reflectance (phenology layout)
  data_phen/output/                           # MSLSP_*.nc per tile
  CDL_data/                                   # cdl_YYYY.tif
  management/                                 # outputs (or point CCMMF_MANAGEMENT at code tree)
    phenology/raw_mslsp_v4.1.2/
    phenology/matched_landiq_mslsp_v4.1.2/
    tillage/ndti_v4.1/
    event_files/
    plant_traits/
```

If you run scripts from `$CCMMF_CODE`, set extract roots explicitly:

```bash
export LANDIQ_GAPFILL_ROOT="$CCMMF_CODE/landiq-gapfill"
export MSLSP_EXTRACT_ROOT="$CCMMF_CODE/mslsp-extract"
export NDTI_EXTRACT_ROOT="$CCMMF_CODE/ndti-extract"
```

These are already set in [ccmmf_env.example.sh](../ccmmf_env.example.sh) when
`CCMMF_CODE` points at `inst/ccmmf`.
---

## 0.4 Environment file (required)

Copy and edit:

```bash
cp "$CCMMF_CODE/../documentation/ccmmf_env.example.sh" "$CCMMF_ROOT/ccmmf_env.sh"
# or from the documentation folder next to this session:
#   documentation/ccmmf_env.example.sh
nano "$CCMMF_ROOT/ccmmf_env.sh"
source "$CCMMF_ROOT/ccmmf_env.sh"
```

**Never rely on BU `/projectnb/...` defaults.** Every script accepts overrides via
`Sys.getenv`; if you skip this file, jobs will look for paths that do not exist on
your machine.

### Critical variables

| Variable | Purpose |
|----------|---------|
| `CCMMF_ROOT` | Top of your data tree |
| `CCMMF_MANAGEMENT` | Outputs + lookups (`event_files`, matched parquets, ...) |
| `CCMMF_LANDIQ_V4` | Harmonized or **gap-filled** LandIQ dir (after gap-fill -> v4.1.2) |
| `CCMMF_LANDIQ_GAPFILL_PRODUCT` | Gap-fill write target (usually `.../LandIQ-harmonized-v4.1.2`) |
| `HLS_IMAGERY_ROOT` / `HLS_IMAGERY_LAYOUT` | NDTI imagery (`phenology` layout for 2020+) |
| `HLSL_BASE` / `HLSS_BASE` | Only if using `HLS_IMAGERY_LAYOUT=flat` |
| `mslsp_new_base` | Directory of `MSLSP_<tile>_<year>.nc` |
| `NDTI_PARCEL_TILEMAP` / `mslsp_parcel_tilemap` | Parcel->tile RDS |
| `COUNTY_TRANSITION_MATRICES_DIR` | County `*_crop_matrix.csv` (full-gap CLASS fill) |
| `EXTERNAL_TRANSITION_MATRIX_CSV` | Statewide transition matrix |
| `TARGET_YEAR` / `PRIOR_YEAR` | Training pair (e.g. 2024 / 2023) |

---

## 0.5 System + R packages

### System (Ubuntu/Debian example)

```bash
sudo apt-get update
sudo apt-get install -y \
  gdal-bin libgdal-dev libproj-dev libgeos-dev libudunits2-dev \
  libnetcdf-dev libcurl4-openssl-dev libssl-dev \
  libxml2-dev libfontconfig1-dev
```

Use your site's equivalent on RHEL/Rocky/conda.

### R packages

```r
install.packages(c(
  "arrow", "dplyr", "data.table", "sf", "terra", "exactextractr",
  "readr", "stringr", "lubridate", "jsonlite", "units"
))
# CDL download only:
# install.packages("CropScapeR")
```

Confirm:

```r
library(arrow); library(sf); library(terra); library(data.table)
packageVersion("arrow")
```

If `arrow` fails to load (`curl_multi_poll`, etc.), fix the system `libcurl` /
reinstall arrow from source on that machine - do not point at another user's
`~/R` library.

---

## 0.6 External dependency - HLS / MSLSP NetCDF

MSLSP extract and NDTI need products from
[Mrina Reddy - HLS_Phenology](https://github.com/mrinareddy/HLS_Phenology):

1. Clone and pin a commit (record the SHA in your run log).
2. Follow that repo to download HLS and produce per-tile `MSLSP_*.nc`.
3. Place NetCDF under `$mslsp_new_base` (default `$CCMMF_ROOT/data_phen/output/<tile>/...`
   or a flat layout your extract config expects).
4. Place reflectance + Fmask under `$HLS_IMAGERY_ROOT` for NDTI.

This repo is **not** vendored into PEcAn; treat it as toolchain Step 0.

---

## 0.7 How to run without SGE

Prefer orchestrators / `Rscript` on one machine (or your own scheduler):

```bash
source "$CCMMF_ROOT/ccmmf_env.sh"

# Gap-fill year pair (after harmonized LandIQ exists):
"$LANDIQ_GAPFILL_ROOT/run_gapfill.sh" "${PRIOR_YEAR},${TARGET_YEAR}"

# MSLSP / NDTI (when extract packages are on $PATH / $CCMMF_CODE):
# "$MSLSP_EXTRACT_ROOT/run_mslsp.sh" $TARGET_YEAR
# "$NDTI_EXTRACT_ROOT/run_ndti.sh" $TARGET_YEAR

# Match + events:
Rscript -e "YEAR <- as.integer(Sys.getenv('TARGET_YEAR')); source(file.path(Sys.getenv('CCMMF_MANAGEMENT'),'scripts/phenology/match_landiq_mslsp.R'))"
Rscript "$CCMMF_MANAGEMENT/scripts/events/make_events_statewide.R" "$TARGET_YEAR"
Rscript "$CCMMF_MANAGEMENT/scripts/events/make_events_statewide.R" "$PRIOR_YEAR"
```

Lab SCC wrappers (`*.sge`) document resource requests (`-l buyin`); translate those to
your cloud's batch system if you parallelize.

Full order: [pipeline.md](../pipeline.md).

---

## 0.8 Smoke checks before Session 1

```bash
source "$CCMMF_ROOT/ccmmf_env.sh"
test -d "$CCMMF_ROOT" && echo "CCMMF_ROOT ok"
test -d "$CCMMF_CODE" && echo "CCMMF_CODE ok"
Rscript -e 'stopifnot(requireNamespace("arrow"), requireNamespace("sf"))'
```

Optional: confirm a year in LandIQ once the harmonized product exists (see pipeline section 4).

---

## 0.9 Checklist

- [ ] Clone PEcAn -> `feature/ccmmf-statewide-monitoring-inst`
- [ ] Create `$CCMMF_ROOT` data layout (no BU paths)
- [ ] Write and `source` `ccmmf_env.sh` from the example
- [ ] Install GDAL stack + R packages; arrow loads cleanly
- [ ] Plan HLS_Phenology install (pinned commit)
- [ ] Know year pair: `PRIOR_YEAR=2023`, `TARGET_YEAR=2024`
- [ ] Proceed to [Session 1](01-landiq.md)

---

## 0.10 Lab appendix (BU SCC only)

| Item | Value |
|------|--------|
| Project data | `/projectnb/dietzelab/ccmmf` |
| R | `module load R/4.4.3` |
| Jobs | `qsub` wrappers with `#$ -l buyin` |
| Flat HLS (pre-2020) | XinyuanJi `State_of_California_HLSL/HLSS` |

Users should ignore this appendix unless collaborating on SCC.
