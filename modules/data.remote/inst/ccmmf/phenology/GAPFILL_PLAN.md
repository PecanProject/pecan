# Gapfill plan: LandIQ 2017 (CDL) + phenology / planting / harvest

**Current focus:** Phenology **date** gap-fill (Phases 2–4) is implemented:
`fit_phenology_gapfill_models.R` + `apply_phenology_gapfill.R` → overlay under
`matched_landiq_mslsp_v4.1.2/gapfill_dates/`. Events load the overlay when present.
See [gapfill/README.md](gapfill/README.md).

Matcher **left-joins** all ag LandIQ parcel-years to MSLSP (`assigned_by = "no_mslsp"`
when retrieval missing). LandIQ crop/ADOY gap-fill product is **v4.1.2**. Combined
MSLSP extract lives under `phenology/raw_mslsp_v4.1.2/`.

This document also describes the **full** long-term scope: empirical gapfill for planting and harvest dates where MSLSP is missing or unmatched, using regressions when ADOY exists and crop-class means when it does not.

**Related code and data**

| Item | Location |
|------|----------|
| LandIQ–MSLSP matching | `scripts/phenology/match_landiq_mslsp.R` |
| Gap audit | `scripts/phenology/gapfill_phase0_audit.R` |
| CDL fractions by parcel | `scripts/cdl/extract_cdl_fractions_by_parcel.R` → `management/cdl/cdl_fractions_year=Y.parquet` |
| Crop metadata (PFT, agricultural flag) | `management/LandIQ_cropCode_lookup_table.csv` |
| Statewide events (phenology, planting, harvest) | `scripts/events/make_events_statewide.R` |
| Trait pools at planting/harvest | `scripts/traits/pool_calculations_from_lookup.R` |
| LandIQ tabular source (downstream) | `$CCMMF_LANDIQ_V4` → **`LandIQ-harmonized-v4.1.2/crops_all_years.parq`** |
| Combined MSLSP (parcel, year, cycle) | `management/phenology/raw_mslsp_v4.1.2/year=Y/mslsp_year=Y.parquet` |
| Date gap-fill fit / apply | `scripts/phenology/fit_phenology_gapfill_models.R`, `apply_phenology_gapfill.R` — [gapfill/README.md](gapfill/README.md) |

---

## Context and constraints

- **2017:** Full-gap LandIQ year is in **v4.1.2** via [landiq-gapfill](../../landiq-gapfill/README.md) (CDL + transition matrix). Point `CCMMF_LANDIQ_V4` at the gap-filled product; no separate stub directory.
- **2016 (and similar):** LandIQ v4.1 has **no ADOY** for 2016; matching uses no-ADOY rules. Gapfill for **dates** may still be needed where MSLSP/HLS outputs are missing.
- **Mike's suggestion:** If ADOY is available, prefer `lm(planting_date ~ ADOY + crop_class + ADOY:crop_class)` and analogous model for harvest; if no ADOY, use **constant mean** planting/harvest by crop class (from training years).

---

## Phase 0 — Audit (prerequisite)

**Goal:** Know exactly which gaps exist before writing fill logic.

**Automation:** `scripts/phenology/gapfill_phase0_audit.R` writes CSVs under  
`phenology/matched_landiq_mslsp_v4.1.2/gapfill_phase0_audit/`.

```bash
module load R/4.4.3
cd scripts/phenology
Rscript gapfill_phase0_audit.R   # main tables (assigned years present on disk)
RUN_LANDIQ_MSLSP_OVERLAP_ONLY=1 Rscript gapfill_phase0_audit.R   # LandIQ vs MSLSP parcel counts only
```

(Optional) `SAVE_PARCEL_YEAR_LONG=1` writes `parcel_year_flags_long.csv` (large).

- [x] **0.1** For years of interest (e.g. 2016–2023), summarize `assigned_year=Y.parquet` under `phenology/matched_landiq_mslsp_v4.1.2/`:
  - Counts by `assigned_by`, `match_outcome`, and key QC fields.
  - For rows with LandIQ crop info: frequency of non-missing `landiq_ADOY`, non-missing `mslsp_OGI` / `mslsp_OGMn` / `mslsp_OGD` (as used in `make_events_statewide.R`).
- [x] **0.2** Cross-check **combined MSLSP** presence: parcels in LandIQ (or CDL) but absent from `raw_mslsp_v4.1.2` for that year.
- [x] **0.3** Document findings — see **Phase 0 results (2026-04-09)** below.

**Deliverable:** CSVs in `gapfill_phase0_audit/` including `parcel_year_gap_summary.csv`, `landiq_crop_rows_pct_nonmissing_mslsp_adoy.csv`, `counts_by_assigned_by.csv`, `counts_by_match_outcome.csv`, `counts_by_qc_column.csv`, `landiq_vs_mslsp_parcel_counts.csv`.

### Phase 0 results (2026-04-09)

**Years on disk:** 2016, 2018–2023. **No** `assigned_year=2017.parquet` (expected until Phase 1). No `raw_mslsp_v4.1.2/year=2017/` combined file in this project snapshot.

**Parcel-year (`parcel_year_gap_summary.csv`):** In every year, essentially all parcel-years in the assigned run have LandIQ crop columns populated (`n_py == n_has_landiq_crop`). Among those, **2016 has 0%** with any season carrying ADOY (`pct_py_adoy_among_crop = 0`); **2018–2023 ~33–41%** have ADOY on at least one crop row. **Matched rows with crop but missing `mslsp_OGI`** are ~0 parcel-years per year except trivial edge cases (**2** in 2016). Same for **harvest metric** (OGMn vs OGD by PFT): **2** parcel-years in 2016 flagged — gapfill volume for “have match + crop but no date” is negligible in current outputs.

**Rows with LandIQ crop info (`landiq_crop_rows_pct_nonmissing_mslsp_adoy.csv`):** On rows where `CLASS`/`SUBCLASS`/`PFT` are present, **~95–97%** have non-missing `mslsp_OGI`, `mslsp_OGMn`, and `mslsp_OGD` (and harvest-metric column per PFT). **2016:** **0%** have `landiq_ADOY`; **2018–2023:** **~42–49%** have ADOY — consistent with Mike’s regression-vs-mean split (no ADOY in 2016 → rely on crop-class means for any date gapfill).

**LandIQ agricultural vs combined MSLSP (`landiq_vs_mslsp_parcel_counts.csv`):** Many ag LandIQ parcels have **no** combined MSLSP row (`n_landiq_only` ~258k–335k/year). After the matcher **left join** (2026), these appear in `assigned_year=*.parquet` as **`assigned_by = "no_mslsp"`** — re-run match and audit to refresh counts.

**Interpretation for later phases:** (1) **2017** LandIQ rows live in **v4.1.2**; still need **MSLSP extract** for 2017. (2) **Regression training** should use **2018–2023** matched rows with non-missing outcomes; **2016** contributes **no** ADOY for `lm(... ~ ADOY * crop_class)`. (3) **`no_mslsp`** parcel-years are the main coverage gap; missing OGI on matched rows remains rare.

---

## Phase 1 — 2017: MSLSP extract + match on v4.1.2

**Goal:** Run MSLSP extract and `match_landiq_mslsp.R` for **2017** using the gap-filled product (no stub).

```bash
export CCMMF_LANDIQ_V4=/projectnb/dietzelab/ccmmf/LandIQ-harmonized-v4.1.2

# MSLSP extract (when NetCDF exists for 2017)
# mslsp-extract/run_mslsp_submit_tiles.sh 2017

# Match (left join — all ag parcel-years, including no_mslsp)
YEAR=2017 Rscript scripts/phenology/match_landiq_mslsp.R

# Audit
cd scripts/phenology && Rscript gapfill_phase0_audit.R
```

**Still required:** `phenology/raw_mslsp_v4.1.2/year=2017/mslsp_year=2017.parquet` from tile combine.

### 1.4 Integration with matching

- [x] **1.4.1** Use **`CCMMF_LANDIQ_V4=LandIQ-harmonized-v4.1.2`** for all years including 2017.
- [ ] **1.4.2** Run assignment for 2017 after MSLSP 2017 exists; confirm `assigned_year=2017.parquet` and `no_mslsp` counts in audit.
- [ ] **1.4.3** Run `make_events_statewide.R 2017` smoke test on matched rows only.

---

## Phase 2 — Training data for planting / harvest models

**Goal:** A clean training set from **good years** (e.g. 2018–2023) where matched rows have both LandIQ crop identity and usable MSLSP dates.

- [ ] **2.1** Load `assigned_year=Y.parquet` for training years; filter `assigned_by == "matched"` and valid crop/PFT per existing event rules.
- [ ] **2.2** Define outcomes (align with `make_events_statewide.R`):
  - **Planting:** calendar date or DOY from `mslsp_OGI` (specify timezone/year rule for DOY).
  - **Harvest:** by PFT—annual row/rice: `mslsp_OGMn`; hay/woody: `mslsp_OGD` (same as harvest filter in events script).
- [ ] **2.3** Predictors: `landiq_ADOY` (numeric), `crop_class` (factor—start with `landiq_CLASS`; optionally `CLASS`+`SUBCLASS` if sample sizes allow).
- [ ] **2.4** Remove rows with missing outcomes; optionally cap extreme DOY for stability.
- [ ] **2.5** Fit:
  - `lm(planting_doy ~ adoy * crop_class)` (or `planting_date` as Date if preferred),
  - `lm(harvest_doy ~ adoy * crop_class)` **per harvest definition** (or separate models by PFT if needed).
- [ ] **2.6** Save fitted models (RDS) and a short coefficient / ANOVA summary for documentation.

**Deliverable:** `scripts/phenology/fit_phenology_gapfill_models.R` (name flexible) + `phenology/gapfill_models/` or `plant_traits/` for RDS outputs.

---

## Phase 3 — Apply gapfill rules

**Goal:** For any target row that needs a planting or harvest **date** and MSLSP is missing, fill using advisor rules; record provenance.

- [ ] **3.1** **If `landiq_ADOY` present** and planting/harvest MSLSP fields missing: `predict()` from Phase 2 models (use same factor levels / reference crop as training).
- [ ] **3.2** **If `landiq_ADOY` missing:** impute planting_doy and harvest_doy from **training-year means by `crop_class`** (and by PFT for harvest metric if you split harvest logic).
- [ ] **3.3** Add columns to a derived table or parquet copy, e.g. `planting_doy_filled`, `harvest_doy_filled`, `gapfill_planting_source`, `gapfill_harvest_source` (`mslsp` | `lm_adoy` | `mean_crop` | `none`).
- [ ] **3.4** Do **not** change canonical `assigned_year=*.parquet` unless the team agrees; prefer a **gapfilled overlay** consumed by a thin wrapper or a fork of `make_events_statewide.R` that prefers filled columns when MSLSP is NA.

**Deliverable:** `scripts/phenology/apply_phenology_gapfill.R` + documented output path.

---

## Phase 4 — Events pipeline hookup

- [ ] **4.1** Extend or duplicate `make_events_statewide.R` logic so planting/harvest events use `planting_doy_filled` / date columns when MSLSP is NA and gapfill flag allows.
- [ ] **4.2** Ensure `initialize_planting()` still receives EVI when available; define behavior when EVI missing (skip planting row vs fallback—align with modeling team).
- [ ] **4.3** Regenerate statewide event files for years that use gapfill; spot-check parcel counts vs audit tables.

---

## Phase 5 — Documentation and handoff

- [ ] **5.1** Update `scripts/phenology/README.md` with a “Gapfilling” section: env vars, script order, 2017 single-season policy, model training years.
- [ ] **5.2** Note **deferred** items explicitly: second MSLSP cycle vs CDL annual label, 2016/2017/2018 harmonization, EMRG/SEN fallbacks (`explore_sen_emrg_matching.R`).
- [ ] **5.3** List limitations for the modeling team (e.g. 2017 = dominant CDL only; no mixture uncertainty in events unless you later add fraction-weighted logic).

---

## Suggested implementation order

**Now:** Phase 1 (2017: CDL with `parcel_id` → `landiq_from_cdl_year=2017.parquet` → wire to matcher / `assigned_year=2017` when MSLSP 2017 exists → `make_events_statewide` smoke test).

**Later:** Phase 2 → Phase 3 → Phase 4 (LM / means / overlay — after 2017 beta path works). Phase 0 (audit) is done; Phase 5 docs when useful.

---

## Open decisions (resolve during Phase 0–1)

- Dominant CDL only vs minimum fraction threshold (e.g. skip parcel if max `frac` &lt; 0.5).
- Exact `season` value for 2017 pseudo-LandIQ (recommended: `2`).
- Whether gapfilled dates are DOY vs Date end-to-end for PEcAn JSON.
- Whether 2016 gets ADOY imputation at all for **matching** vs only **event** date gapfill (advisor leaned toward LM/means for dates, not necessarily imputing ADOY first).

---

## Checklist summary

| Phase | Theme | Status |
|-------|--------|--------|
| 0 | Audit gaps in assigned + MSLSP | ☐ |
| 1 | 2017 CDL → pseudo-LandIQ + match + events smoke test | ☐ |
| 2 | Fit LM + compute crop-class means (training years) | ☐ |
| 3 | Apply gapfill + provenance columns | ☐ |
| 4 | Wire events to filled dates | ☐ |
| 5 | README + handoff notes | ☐ |
