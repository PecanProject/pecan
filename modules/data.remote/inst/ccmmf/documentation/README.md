# CCMMF monitoring pipeline - documentation

**Start here.** This folder is the **user / training** entry point for the CCMMF
LandIQ -> phenology -> management-events monitoring workflow.

Users run on **their own cloud / HPC**, not BU SCC. Begin with
[Session 0 - Environment](sessions/00-environment.md).

**PEcAn home:** [PR #3913](https://github.com/PecanProject/pecan/pull/3913) -
`feature/ccmmf-statewide-monitoring-inst` -> `modules/data.remote/inst/ccmmf/`.

**Delivery vs training**

| What | Years | Notes |
|------|-------|--------|
| **Delivered product** | Gap-filled **2016-2023** (v4.1.2) + match + events | Historical closeout (lab) |
| **Training walkthrough** | **`TARGET_YEAR=2024`** + rerun **`PRIOR_YEAR=2023`** | Year-pair after a new CADWR release |

Do not treat 2024 examples as already-on-disk until LandIQ 2024 is downloaded,
harmonized, and gap-filled.

---

## What to read (users / training)

Read in order. Everything a trainee needs for the walkthrough lives **in this
`documentation/` folder**.

| Order | Document | Purpose |
|------:|----------|---------|
| 0 | [sessions/00-environment.md](sessions/00-environment.md) | Clone repos, deps, data root, `ccmmf_env` |
| - | [ccmmf_env.example.sh](ccmmf_env.example.sh) | Portable env template (copy -> edit -> `source`) |
| spine | [pipeline.md](pipeline.md) | End-to-end year-pair run order, env vars, checklist |
| 1 | [sessions/01-landiq.md](sessions/01-landiq.md) | LandIQ download, harmonize, gap-fill |
| 2 | [sessions/02-phenology.md](sessions/02-phenology.md) | HLS/MSLSP, match, traits, planting/harvest events |
| 3 | [sessions/03-tillage-fertilizer.md](sessions/03-tillage-fertilizer.md) | NDTI tillage (+ fert notes) |
| 4 | [sessions/04-irrigation.md](sessions/04-irrigation.md) | Irrigation (parallel track) |

**[pipeline.md](pipeline.md)** is the technical spine for Sessions 1-3 (LandIQ ->
gap-fill -> HLS -> MSLSP -> match -> events). Session 4 runs in parallel on the
same parcel geometry.

### Geometry harmonization (separate repo)

Session 1 also uses **[ccmmf/cadwr-landuse](https://github.com/ccmmf/cadwr-landuse)**
(Python / pixi). That repo's README and `docs/` are the algorithm reference;
Session 1 only covers the CCMMF ops sequence.

---

## Package READMEs (not a second curriculum)

Next to the code under `inst/ccmmf/` (or the lab `management/` mirror) each
workflow has a **package README** (flags, QC, troubleshooting). Those are
**linked from the sessions** when you need detail - they are not alternate
training paths and are not listed as peer docs above.

Examples (relative to this folder):

| Session | Package README |
|---------|----------------|
| 1 | [../landiq-gapfill/README.md](../landiq-gapfill/README.md) |
| 2 | [../mslsp-extract/README.md](../mslsp-extract/README.md), [../phenology/match/README.md](../phenology/match/README.md), [../traits/README.md](../traits/README.md), [../events/README.md](../events/README.md) |
| 2-3 | [../hls/README.md](../hls/README.md) (parcel-tile map / shared HLS helpers) |
| 3 | [../ndti-extract/README.md](../ndti-extract/README.md), [../tillage/README.md](../tillage/README.md) |

On the lab SCC tree, some of the same docs still live under `management/scripts/`
(e.g. `scripts/phenology/match/`); prefer the paths in the PEcAn `inst/ccmmf`
layout when cloning from GitHub.

---

## How this folder is organized

| Path | Role |
|------|------|
| `README.md` (this file) | User entry point and reading order |
| `ccmmf_env.example.sh` | Env template for Session 0 |
| `pipeline.md` | Year-processing spine (used with Sessions 1-3) |
| `sessions/00-04.md` | Training sessions (background, walkthrough, verify) |

Internal planning notes (`*_PLAN.md`, etc.) are not part of this documentation
product.
