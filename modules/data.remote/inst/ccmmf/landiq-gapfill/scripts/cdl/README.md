# CDL for LandIQ gap-fill

USDA NASS Cropland Data Layer (CDL) supplies **parcel-level crop code fractions** for gap-fill emission tables and crop identity inference.

**Pipeline:** step 0 of [`run_gapfill.sh`](../../run_gapfill.sh) (see [`../../README.md`](../../README.md)).

## Role

1. **Download** California GeoTIFF per year → `$CDL_DIR/cdl_YYYY.tif`
2. **Extract** area-weighted CDL code fractions per parcel → `$CDL_OUT_DIR/cdl_fractions_year=YYYY.parquet`
3. **Consume** in gap-fill: overlapping LandIQ + CDL parcel-years train emission tables (`01_build_lookup.R`, `02_build_probs.R`); per-year fractions drive crop/subclass fill.

CDL-related **QC CSVs** (subclass lookup coverage, codes seen) are written by the emission step to `$LANDIQ_GAPFILL_ROOT/outputs/`, not under `cdl/`.

## Environment

| Variable | Default | Role |
|----------|---------|------|
| `LANDIQ_GAPFILL_ROOT` | auto-detected | Package root |
| `CDL_DIR` | `$CCMMF_ROOT/CDL_data` | Raw GeoTIFF rasters (large; stays at repo root) |
| `CDL_OUT_DIR` | `$LANDIQ_GAPFILL_ROOT/cdl` | Fraction parquets |
| `CCMMF_LANDIQ_V4` | (required) | Harmonized LandIQ dir; extract reads its `parcels-consolidated.gpkg` for parcel geometry |

## Production workflow

Getting CDL for a new year (here, 2024):

```bash
module load R/4.4.3
export LANDIQ_GAPFILL_ROOT=$CCMMF_ROOT/management/landiq-gapfill

# Normal run fetches CDL for the year automatically (skips if already present)
$LANDIQ_GAPFILL_ROOT/run_gapfill.sh 2024

# CDL only (skip the gap-fill steps)
$LANDIQ_GAPFILL_ROOT/run_gapfill.sh --no-crop --no-adoy --no-product 2024

# Force a re-download + re-extract for the year
$LANDIQ_GAPFILL_ROOT/run_gapfill.sh --rebuild-cdl --no-crop --no-adoy --no-product 2024
```

### Manual CLIs

```bash
export CDL_DIR=$CCMMF_ROOT/CDL_data
export CDL_OUT_DIR=$LANDIQ_GAPFILL_ROOT/cdl
export CCMMF_LANDIQ_V4=$CCMMF_ROOT/LandIQ-harmonized-v4.1

Rscript download_cdl_nass.R 2024
Rscript extract_cdl_fractions_by_parcel.R 2024
```

### SGE (heavy extract / download)

```bash
qsub -v YEAR=2024 $LANDIQ_GAPFILL_ROOT/sge/extract_cdl_fractions.sge
qsub -v "YEARS=2024" $LANDIQ_GAPFILL_ROOT/sge/download_cdl_nass.sge
```

Logs: `landiq-gapfill/sge_logs/`.

## Outputs

| Location | Artifact |
|----------|----------|
| `$CDL_OUT_DIR` | `cdl_fractions_year=YYYY.parquet` |
| `$LANDIQ_GAPFILL_ROOT/outputs/` | `cdl_landiq_subclass_coverage_*.csv`, `cdl_codes_seen_*.csv` (emission build) |

Parquet columns: `parcel_id`, `year`, `cdl_code`, `frac`, `w_total`, `sum_w2`. Fractions sum to 1 per parcel.

## Scripts

| Script | Purpose |
|--------|---------|
| `download_cdl_nass.R` | CropScape download (California) |
| `extract_cdl_fractions_by_parcel.R` | Zonal extract to parquet |

## References

- NASS CDL legend: [CropScape FAQ](https://www.nass.usda.gov/Research_and_Science/Cropland/sarsfaqs2.php)
- Code names: `landiq-gapfill/data/cdl_nass_cropland_code_lookup.csv`
- Ag filter: `landiq-gapfill/data/LandIQ_cropCode_lookup_table.csv`
