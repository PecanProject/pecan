# CCMMF monitoring pipeline - documentation

**Start here.** This folder is the user entry point for the CCMMF LandIQ ->
phenology -> management-events monitoring workflow.

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

Developers maintaining scripts should also see the [repo index](../README.md) for
per-step operator READMEs beside the code.

---

## Training sessions

| Session | Topic | Document | Operator reference |
|---------|--------|----------|-------------------|
| **0** | **Environment setup (portable)** | [sessions/00-environment.md](sessions/00-environment.md) | [ccmmf_env.example.sh](ccmmf_env.example.sh) |
| **1** | LandIQ harmonization + gap-fill | [sessions/01-landiq.md](sessions/01-landiq.md) | [landiq-gapfill/README.md](../landiq-gapfill/README.md) |
| **2** | Phenology, traits, planting/harvest events | [sessions/02-phenology.md](sessions/02-phenology.md) | [mslsp-extract](../mslsp-extract/README.md), [match](../scripts/phenology/match/README.md), [traits](../scripts/traits/README.md), [events](../scripts/events/README.md) |
| **3** | Tillage (NDTI) + fertilization (N rates) | [sessions/03-tillage-fertilizer.md](sessions/03-tillage-fertilizer.md) | [ndti-extract](../ndti-extract/README.md), [tillage](../scripts/tillage/README.md); fert under `usr/akash/...` (vendoring TBD) |
| **4** | Irrigation (statewide water balance) | [sessions/04-irrigation.md](sessions/04-irrigation.md) | Alexey irrigation-statewide under PEcAn; anchor-site Python under `irrigation/` |

---

## Full pipeline (Sessions 1-3 core)

**[pipeline.md](pipeline.md)** - end-to-end run order, environment variables, commands,
and a year-processing checklist (LandIQ -> gap-fill -> HLS -> MSLSP -> match -> events).

Session 4 (irrigation) runs in parallel on the same parcel geometry; see
[sessions/04-irrigation.md](sessions/04-irrigation.md).

---

## How this folder is organized

| Path | Audience | Purpose |
|------|----------|---------|
| `README.md` (this file) | CARB / users | Entry point and session index |
| `ccmmf_env.example.sh` | Everyone | Portable env template (copy -> edit -> `source`) |
| `pipeline.md` | Operators | Technical spine for crop/phenology/tillage year processing |
| `sessions/00-environment.md` | Users | Clone, deps, paths, non-SGE runs |
| `sessions/01-04.md` | Training | Session narratives (background, walkthrough, verify) |
| `../mslsp-extract/README.md` | Operators | MSLSP extraction (Session 2) |
| `../ndti-extract/README.md` | Operators | NDTI extraction (Session 3) |
| `../landiq-gapfill/README.md`, `../scripts/hls/README.md` | Operators | Gap-fill, parcel-tile map, shared HLS framework |

Internal planning notes (`*_PLAN.md`, dev indexes) live under `scripts/` and are not
part of this documentation product.
