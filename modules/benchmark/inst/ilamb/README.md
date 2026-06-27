# PEcAn-to-ILAMB Conversion

Converts PEcAn SDA carbon reanalysis outputs (downscaled GeoTIFF ensemble maps)
into ILAMB-compatible CF-convention netCDF files for benchmarking against
TRENDY, CMIP, and observational datasets.

## Overview

The PEcAn North American carbon reanalysis (Zhang et al. 2026) provides
downscaled 1 km ensemble maps of four state variables. This tool reads those
GeoTIFFs, computes the ensemble mean, coarsens to ILAMB's default 0.5 degree
resolution, applies unit conversions, and writes CF-1.8 compliant netCDF that
ILAMB can ingest directly.

## Input

GeoTIFF ensemble maps organized as:

```
<input_dir>/<year>/<variable>_<year>/ensemble_<n>_<year>_<variable>.tiff
```

- 13 years (2012-2024), annual snapshots fixed to July 15
- 4 variables, 100 ensemble members each
- 1 km resolution (9360 x 19080), EPSG:4326

## Variable mapping and unit conversions

| PEcAn variable | CMOR name | Source units | Conversion | ILAMB units |
|----------------|-----------|--------------|------------|-------------|
| AbvGrndWood    | cVeg      | Mg C ha-1    | x 0.1      | kg m-2      |
| TotSoilCarb    | cSoil     | kg C m-2     | none       | kg m-2      |
| SoilMoistFrac  | mrsol     | vol. percent | x 9.98     | kg m-2      |
| LAI            | lai       | m2 m-2       | none       | m2 m-2      |

**Aboveground biomass** is already a carbon density (Mg C ha-1), so the
conversion to kg m-2 is purely unit scaling: 1 Mg ha-1 = 0.1 kg m-2.

**Soil moisture** is volumetric water content expressed as percent over the
0-100 cm root zone. Conversion to mass per area:

```
kg m-2 = percent / 100 x 1.0 m depth x 998 kg/m3  =  percent x 9.98
```

The 0-100 cm root-zone depth was confirmed with the dataset author
(D. Zhang, pers. comm.), and matches the depth span of the ILAMB Wang2021
soil-moisture benchmark (0-10, 10-30, 30-50, 50-100 cm layers).

## Output

CF-1.8 compliant netCDF on a 0.5 degree regular grid (156 x 318) covering the
North American study area (7-85N, 179-20W):

- `<output_dir>/<cmor_name>/<cmor_name>_<year>.nc` (one file per year)
- `<output_dir>/<cmor_name>.nc` (merged multi-year file)

Latitude is monotonically increasing (south to north); coordinates are rounded
to 0.01 degrees; time is encoded as days since 1850-01-01 with full-year
bounds.

## Usage

```bash
module load python3 gcc/13.2.0
export PATH=$HOME/.local/bin:$PATH

# Convert all variables, all years
python convert_geotiff_to_ilamb.py \
    --input_dir /path/to/NA_SDA_maps_zipped \
    --output_dir /path/to/output

# Single variable / year range
python convert_geotiff_to_ilamb.py --variables AbvGrndWood --years 2014 2014

# Skip the merge step
python convert_geotiff_to_ilamb.py --skip-merge
```

On an HPC system, reading 100 full-resolution members per variable exceeds
interactive CPU limits; submit the full run as a batch job.

## Testing

```bash
pytest test_convert.py -v
```

13 tests cover file existence, CMOR variable naming, all four unit
conversions, output grid shape, CF-1.8 compliance, latitude direction,
spatial coverage, chronological multi-year merging, and ILAMB `ModelResult`
loading. Set `ILAMB_OUTPUT_DIR` to point the tests at your output directory.

## Dependencies

`numpy`, `xarray`, `rasterio`, `netCDF4`, and `ILAMB` (for the loading test).

## Notes

- Fluxes (GPP, NEE) are not included; the downscaled product covers only the
  four state variables above. Flux benchmarking will draw from the raw SDA
  netCDF outputs in a later contribution.
- A known structural discontinuity exists in the underlying LandTrendr input
  around 2017-2018 (see the ORNL DAAC documentation); it is preserved as-is in
  the converted output rather than adjusted here.

---

# Multi-Model Benchmarking (CMIP6 and TRENDY)

Building on the conversion pipeline above, these scripts benchmark the PEcAn
reanalysis against two major model intercomparison ensembles, CMIP6 and TRENDY
(Global Carbon Budget), using ILAMB, and score the individual PEcAn ensemble
members so that PEcAn's skill spread can be compared directly with the model
ensembles' spread.

Comparison is over North America for three state variables with established
observational benchmarks: vegetation carbon (`cVeg`), soil carbon (`cSoil`),
and leaf area index (`lai`). Soil moisture is supported by the conversion step
but is not yet included in the multi-model scoring (see Scope below).

## Pipeline

Run in order; each step writes inputs for the next. The first script is the
conversion tool documented above.

| Step | Script | Purpose |
|------|--------|---------|
| 1 | `convert_geotiff_to_ilamb.py` | PEcAn GeoTIFF ensemble maps to CF netCDF |
| 2 | `build_cmip6_ensemble.py` | Download + regrid CMIP6 historical fields |
| 3 | `build_cmip6_ssp.py` | Splice CMIP6 historical + ssp245 for a longer record |
| 4 | `build_trendy_ensemble.py` | Download + regrid TRENDY (GCB) fields |
| 5 | `build_window_ensembles.py` | Slice all models to an evaluation window; build ensemble means |
| 6 | `build_pecan_members.py` | Per-member PEcAn fields for spread analysis |
| 7 | `make_spread_figures.py` | Spread figure from ILAMB scores |

All model fields are regridded onto the same 0.5 degree North American grid the
conversion step produces, so PEcAn, the observational benchmarks, and every
model share one grid. LAI is reduced to its July value throughout, to match the
July snapshot of the PEcAn product. The TRENDY ensemble mean is built only from
members that pass a per-variable physical-plausibility screen; the members used
are recorded in `trendy_ensemble_manifest.json`.

## Evaluation windows

The PEcAn reanalysis and the models are compared over two windows, which answer
different questions:

- **2012-2014**: a mean-state snapshot using the full set of CMIP6 historical
  models (25), the broadest representative model sample.
- **2015-2023**: a longer record for interannual variability and trends, using
  the CMIP6 models that provide a continuous historical + ssp245 land-carbon
  record (14).

Reporting both is deliberate: the longer window supports variability and trend
analysis that three years cannot, while the 2012-2014 window retains the full
model sample. The two windows also differ in CMIP6 composition, and that
difference is itself informative; see the note on soil carbon below.

## ILAMB configuration

`pecan_ilamb.cfg` defines three confrontations:

| Variable | Benchmark | Notes |
|----------|-----------|-------|
| Biomass (cVeg) | Xu & Saatchi 2021 | RMSE skipped; mass-weighted |
| Leaf Area Index | GIMMS LAI4g | seasonal-cycle and RMSE skipped |
| Soil Carbon | HWSD2 | RMSE skipped; mass-weighted |

The configuration applies no time or region restriction itself; the evaluation
window is set entirely by the windowed input files (step 5), and the North
American extent is set by the grid.

## Results

ILAMB overall scores (0-1, higher is a closer match to the benchmark). PEcAn is
the reanalysis ensemble mean; CMIP6 and TRENDY are ensemble means.

**2012-2014 (25 CMIP6 models)**

| Variable | PEcAn | CMIP6 | TRENDY |
|----------|-------|-------|--------|
| Biomass | 0.483 | 0.477 | 0.410 |
| Leaf Area Index | 0.510 | 0.448 | 0.453 |
| Soil Carbon | 0.625 | 0.634 | 0.478 |

**2015-2023 (14 CMIP6 models)**

| Variable | PEcAn | CMIP6 | TRENDY |
|----------|-------|-------|--------|
| Biomass | 0.464 | 0.473 | 0.409 |
| Leaf Area Index | 0.513 | 0.448 | 0.454 |
| Soil Carbon | 0.648 | 0.715 | 0.478 |

PEcAn scores above both model ensembles on leaf area index in every window, and
above the TRENDY ensemble on all three variables. On biomass and soil carbon it
is comparable to the CMIP6 ensemble.

The higher CMIP6 soil-carbon score in the 2015-2023 window (0.715) is a
composition effect, not a change in the benchmark: the 14 models with a
continuous ssp245 record happen to be stronger soil-carbon performers. The full
25-model window (0.634) is the representative figure, where PEcAn and CMIP6 are
close.

## Ensemble spread

Scoring the 100 individual PEcAn members alongside the individual CMIP6 and
TRENDY models shows that the PEcAn members vary far less in skill than the
models do. Standard deviation of member scores:

| Variable | PEcAn (100) | CMIP6 | TRENDY |
|----------|-------------|-------|--------|
| Biomass (2012-2014) | 0.003 | 0.057 | 0.086 |
| LAI (2012-2014) | 0.004 | 0.082 | 0.145 |
| Soil Carbon (2012-2014) | 0.003 | 0.166 | 0.137 |
| Biomass (2015-2023) | 0.014 | 0.040 | 0.086 |
| LAI (2015-2023) | 0.002 | 0.085 | 0.147 |
| Soil Carbon (2015-2023) | 0.001 | 0.146 | 0.137 |

The narrow PEcAn spread reflects skill, not identical members: the members
differ in their spatial carbon fields (for soil carbon, on the order of one
percent of the field, member to member) yet match the benchmark about equally
well. Whether that spread is appropriately sized relative to the error, that is,
whether the ensemble is well-calibrated, is a separate question, addressed by
probabilistic scoring in a later contribution.

`fig_spread_clouds.png` shows the per-member scores for all three ensembles
across both windows.

## Scope and caveats

- **North America only.** The comparison uses the North American PEcAn grid.
- **Biomass pool mismatch.** PEcAn `cVeg` is above-ground wood carbon, while the
  Xu & Saatchi benchmark is total live biomass (including roots). PEcAn is
  therefore expected to read somewhat low on biomass by construction.
- **Soil-carbon depth mismatch.** PEcAn soil carbon integrates 0-200 cm, deeper
  than the benchmark, so PEcAn reads higher in deep-carbon regions.
- **Soil moisture deferred.** The conversion step handles soil moisture, but
  multi-model soil-moisture scoring is not yet included.
- **Probabilistic scoring is future work.** The spread results above motivate
  ensemble calibration metrics (rank histograms, reliability), which are a
  planned follow-on rather than part of this contribution.

## Reproducing the figure

`make_spread_figures.py` reads ILAMB `scores.csv` output for the two windows.
Its paths refer to the analysis working tree where the scoring runs were
produced; point them at your own ILAMB build directories to regenerate the
figure.

## Data sources

- CMIP6: ESGF, via `intake-esgf` (historical and ssp245, r1i1p1f1).
- TRENDY: Global Carbon Budget 2024, public download index.
- Benchmarks: Xu & Saatchi 2021 (biomass), GIMMS LAI4g (leaf area index),
  HWSD2 (soil carbon).
