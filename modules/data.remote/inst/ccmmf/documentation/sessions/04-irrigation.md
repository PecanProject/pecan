# Training Session 4 - Irrigation

This session covers how CCMMF derives **irrigation event files** from evapotranspiration,
precipitation, and soil water-balance logic, and how those events connect to the rest of
the monitoring pipeline (alongside planting, harvest, phenology, tillage,
N fertilization, and organic amendments).

**Navigation:** [Pipeline](../pipeline.md) | [Session 3](03-tillage-fertilizer.md)

**Note:** Irrigation is a parallel statewide workflow (Alexey Shiklomanov) that
uses the same harmonized LandIQ **parcel_id** geometry as the crop/phenology
pipeline. Operator docs: [pipeline.md](../pipeline.md) Session 4;
`workflows/irrigation-statewide/` in the PEcAn clone the session lead provides.

---

## 4.1 Two implementations

| Track | Scope | Canonical doc |
|-------|--------|---------------|
| **Statewide (production)** | ~600k LandIQ parcels; `targets` pipeline | `workflows/irrigation-statewide/README.md` in the PEcAn clone the session lead provides |
| **Anchor-site prototype** | Design points (`id`, `lat`, `lon`); OpenET API + CHIRPS | Session lead / package Python README if present |

Use the **statewide workflow** for CARB-scale monitoring. The `management/irrigation/`
Python stack is an earlier site-based prototype (Katherine Rein, Spring 2025) useful for
understanding data sources and water-balance event file format.

---

## 4.2 Statewide irrigation

**Location:** PEcAn workflow
`workflows/irrigation-statewide/` (clone the branch/repo the session lead names;
see that folder's README).

**What it does:** Generates PEcAn-format **irrigation event files** for all California
agricultural parcels using a reproducible **`targets`** pipeline.

### Setup

Run from the PEcAn workflow root that contains `workflows/irrigation-statewide`.
Set in `.Renviron`:

```
TAR_CONFIG=workflows/irrigation-statewide/_targets.yaml
```

Edit `config_paths.yml` so data roots point at **your** `$CCMMF_ROOT` (or equivalent).

### Run

Three configurations in `config.yml`:

| Config | Parcels | Use case |
|--------|---------|----------|
| `small` (default) | 1,000 (batches of 100) | Local test |
| `medium` | 10,000 (batches of 1,000) | Parallel workers (~15) |
| `all` | ~600,000 (batches of 5,000) | Full statewide (~60 workers) |

```bash
TAR_PROJECT=all Rscript -e "targets::tar_make()"
```

Full details: `workflows/irrigation-statewide/README.md` in the PEcAn clone the session lead provides.

### Input preprocessing

Raster inputs (CHIRPS precipitation, CIMIS reference ET, SSURGO soil properties) are
preprocessed to parcel-level time series before the main workflow:

`workflows/irrigation-statewide/preprocessing/README.md` (same clone)

| Dataset | Scripts |
|---------|---------|
| CHIRPS daily precipitation | `preprocessing/chirps-preprocess.R` |
| CIMIS ETref | `cimis-01-weights.R` -> `cimis-02-extract.R` -> `cimis-03-combine.sql` |
| SSURGO soil AWC | `ssurgo-01-spatial-weights.R` -> `ssurgo-02-combine.R` |

CHIRPS raw files for the anchor-site prototype also live under
`management/irrigation/chirps-v2.0.*.days_p05.nc`.

### Verify

```bash
# from irrigation-statewide/
Rscript check-result.R
```

Statewide outputs are written under Alexey's event-outputs tree (see `config_paths.yml`);
preprocessed irrigation parquet is consumed by SIPNET restart workflows in the same PEcAn
fork.

---

## 4.3 Anchor-site prototype (`management/irrigation/`)

**Operator doc:** [README.txt](../../irrigation/pecan/modules/data.remote/inst/Python/README.txt)

**Data sources:**

- **Evapotranspiration:** [OpenET](https://openet.gitbook.io/docs) (API or Google Earth Engine)
- **Precipitation:** [CHIRPS](https://data.chc.ucsb.edu/products/CHIRPS-2.0/)

**Main Python entry points** (`irrigation/pecan/modules/data.remote/inst/Python/`):

| Script | Role |
|--------|------|
| `CCMMF_Irrigation_API.py` | Download + update water balance via OpenET API |
| `CCMMF_Irrigation_DataDownload.py` | ET, CHIRPS, and per-location download helpers |
| `CCMMF_Irrigation_CalcVis.py` | Water balance + time-series plots |
| `CCMMF_Irrigation_Events.py` | Weekly irrigation event txt files per location |

**Key folders under `management/irrigation/`:**

| Folder | Contents |
|--------|----------|
| `WaterBalanceCSV/` | Per-location CSV backups |
| `CCMMF_Irrigation_Parquet/` | Hive-partitioned parquet (location x year) |
| `CCMMF_Irrigation_EventFiles/` | `irrigation_eventfile_{location_id}.txt` |

**Scale up:** replace `design_points.csv` (columns `id`, `lat`, `lon`) with a parcel
location table. The statewide `targets` workflow supersedes this for production.

**Known open items** (from README.txt): time-series validation, weekly aggregation QA,
site-specific water holding capacity and crop rooting depth.

---

## 4.4 Connect to the rest of the monitoring pipeline

Sessions 1-3 produce **statewide** planting, harvest, phenology, and tillage
events under `management/event_files/`. Irrigation from the statewide workflow is
another event type for the SIPNET ecosystem model (alongside N fert / NCC from
Session 3).

Merge arbitrary event tables into one PEcAn JSON bundle:

```bash
Rscript $CCMMF_CODE/events/combine_management_events_pecan.R \
  --planting events_planting.csv \
  --harvest events_harvest.csv \
  --tillage events_tillage.csv \
  --irrigation events_irrigation.csv \
  --out event_files/combined_events_pecanFormat.json
```

See [events/README.md](../../events/README.md) and the header in
`combine_management_events_pecan.R` for input schemas.

Alexey's SIPNET (ecosystem model) restart workflow also loads irrigation parquet
directly - see the irrigation / SIPNET restart workflow scripts (session lead).

---

## 4.5 Hands-on checklist

**Statewide (recommended)**

- [ ] Read `workflows/irrigation-statewide/README.md` in your PEcAn clone.
- [ ] Confirm preprocessing inputs exist (CHIRPS, CIMIS, SSURGO - see preprocessing README).
- [ ] Run `TAR_PROJECT=small` smoke test locally.
- [ ] Inspect output with `check-result.R` before `TAR_PROJECT=all`.

**Anchor-site (optional)**

- [ ] Set up `ccmmf_env` conda env (see Python README.txt).
- [ ] Authenticate Google Earth Engine + OpenET.
- [ ] Run `CCMMF_Irrigation_API.py` for a design point; inspect `CCMMF_Irrigation_EventFiles/`.

---

## 4.6 Pipeline map (all four sessions)

```mermaid
flowchart TD
  S1["Session 1\nLandIQ + gap-fill"]
  S2["Session 2\nMSLSP + match + events"]
  S3["Session 3\nNDTI tillage + N rates"]
  S4["Session 4\nIrrigation water balance"]
  COMB["combine_management_events_pecan.R\nor SIPNET restart workflow"]

  S1 --> S2
  S2 --> S3
  S1 --> S4
  S2 --> COMB
  S3 --> COMB
  S4 --> COMB
```
