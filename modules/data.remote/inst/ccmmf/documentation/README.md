# CCMMF monitoring pipeline - documentation

**You are here to process a new LandIQ year.**

This training walkthrough adds **2024** and re-runs **2023** (the year pair).
You work on **your own Linux cloud / HPC** (not BU SCC).

| Variable | Value in this training |
|----------|-------------------------|
| `TARGET_YEAR` | **2024** (new CADWR / LandIQ release) |
| `PRIOR_YEAR` | **2023** (re-gap-fill, rematch, rebuild events with the new series) |

When CADWR releases a later year, use the same steps with
`TARGET_YEAR=<new>` and `PRIOR_YEAR=<new-1>`.

**Code on GitHub:** [PR #3913](https://github.com/PecanProject/pecan/pull/3913) -
branch `feature/ccmmf-statewide-monitoring-inst` ->
`modules/data.remote/inst/ccmmf/`.

---

## Start here (read in order)

| Step | Open | You will |
|-----:|------|----------|
| 0 | [sessions/00-environment.md](sessions/00-environment.md) | Clone code, install R (+ pixi for Python), create `$CCMMF_ROOT`, `source` your env file |
| - | [ccmmf_env.example.sh](ccmmf_env.example.sh) | Copy to `$CCMMF_ROOT/ccmmf_env.sh` and set paths / years |
| 1 | [sessions/01-landiq.md](sessions/01-landiq.md) | Download LandIQ **2024**, harmonize geometry, gap-fill **2023,2024** |
| spine | [pipeline.md](pipeline.md) | Full year-pair checklist after LandIQ (HLS -> MSLSP/NDTI -> match -> events) |
| 2 | [sessions/02-phenology.md](sessions/02-phenology.md) | Phenology extract, match, planting/harvest events for **2023 and 2024** |
| 3 | [sessions/03-tillage-fertilizer.md](sessions/03-tillage-fertilizer.md) | NDTI tillage (opt-in); fert notes |
| 4 | [sessions/04-irrigation.md](sessions/04-irrigation.md) | Irrigation (parallel; same parcels) |

**Two languages, two repos**

| Part | Language / tool | Repo |
|------|-----------------|------|
| Harmonize LandIQ parcels | **Python** via **pixi** | [ccmmf/cadwr-landuse](https://github.com/ccmmf/cadwr-landuse) (`feature/auto-discover-landiq-years` until on `main`) |
| Gap-fill, extract, match, events | **R** (`Rscript` / `run_*.sh`) | PEcAn `inst/ccmmf/` (this PR) |

Session write-ups are the training path. Package READMEs next to the code
(`landiq-gapfill/`, `mslsp-extract/`, …) are optional detail (flags, QC) linked
from sessions - not a second curriculum.

---

## What "done" looks like for this training

After Sessions 0-2 (core):

1. LandIQ **2024** is in your harmonized crop table and gap-filled product.
2. **2023** and **2024** are gap-filled in `LandIQ-harmonized-v4.1.2` (or your writable copy).
3. Matched MSLSP + planting/harvest/phenology events exist for **both** years.

Lab note: a historical **2016-2023** product already exists on BU SCC. Your job in
training is the **year-pair update**, not rebuilding all history from scratch.
