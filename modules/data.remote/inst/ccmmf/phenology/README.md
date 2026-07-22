# Phenology scripts

Utilities for MSLSP–LandIQ assignment and QC. Gap-fill lives in
[`landiq-gapfill/`](../../landiq-gapfill/README.md) (not this folder).

| Doc | What |
|-----|------|
| **[`match/README.md`](match/README.md)** | **Match LandIQ seasons → MSLSP cycles** (step 6) |
| [`../events/README.md`](../events/README.md) | Statewide event files from matched seasons |
| [`../traits/README.md`](../traits/README.md) | Planting/harvest pools from matched EVI + lookups |
| [`../tillage/README.md`](../tillage/README.md) | Tillage metrics from NDTI + matched phenology |

## Other scripts in this folder

| Script | Purpose |
|--------|---------|
| `match_landiq_mslsp.R` | Assignment (see [`match/README.md`](match/README.md)) |
| `match_landiq_mslsp.sge` | SGE wrapper (`qsub -v YEAR=Y`) |
| `build_qc_report.R` | Narrative QC report across assigned years |
| `qc_filter_examples.R` | Example filters on assigned parquet |
| `gapfill/` | Legacy pointers → use `landiq-gapfill/` |
