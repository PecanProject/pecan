# ESA CCI Biomass Benchmark Extension

Adds the ESA CCI Biomass v7.0 product as a second aboveground-biomass benchmark
for the PEcAn carbon reanalysis, and uses it to test the ensemble calibration
against contemporary observations and against observation error.

The existing biomass benchmark (XuSaatchi) is frozen near 2010 and carries no
uncertainty layer, while the reanalysis runs through 2024. ESACCI provides annual
maps for 2005-2012 and 2015-2024 with a per-pixel standard deviation, which lets
us (1) score the ensemble against 2020 and 2024 observations that match the
reanalysis period, and (2) propagate observation error into the calibration.

## Contents

- `convert_esacci_to_ilamb.py` converts the ESACCI aggregated GeoTIFF to an
  ILAMB-compatible NetCDF matching the XuSaatchi grid and units. It extracts the
  requested years, regrids 0.25 to 0.5 degree, and applies the IPCC woody carbon
  fraction (0.47) to convert dry biomass to carbon density, since XuSaatchi and
  the model output are in carbon. It writes `biomass` and `biomass_sd` variables.
- `score_esacci.py` samples the downscaled ensemble at the assimilation sites and
  computes the calibration diagnostics against ESACCI and XuSaatchi, then adds
  observation error to the ensemble spread and re-tests. Its
  `coverage_with_obs_error` and `spread_skill_with_obs_error` functions are
  general and apply to any benchmark with an uncertainty estimate.
- `test_esacci.py` covers the year-to-band mapping, the carbon conversion, and the
  observation-error diagnostics, using only NumPy and the calibration module.

## Data

ESA CCI Biomass v7.0 (Santoro & Cartus, 2026), Open Access, from CEDA:
https://data.ceda.ac.uk/neodc/esacci/biomass/data/agb/maps/v7.0/geotiff/aggregated

Download the aggregated 25 km files (a few MB each):
- `ESACCI-BIOMASS-L4-AGB-MERGED-25000m-fv7.0.tif` (biomass)
- `ESACCI-BIOMASS-L4-AGB_SD-MERGED-25000m-fv7.0.tif` (uncertainty)

and save them as `ESACCI_AGB_25km.tif` and `ESACCI_AGB_SD_25km.tif` next to the
converter. The full-resolution product is 760 GB and is not needed; the 25 km
aggregate is sufficient for half-degree benchmarking.

## Units note

ESACCI reports dry above-ground biomass; XuSaatchi's map is carbon density and the
PEcAn AbvGrndWood output is carbon. Converting biomass to carbon with the standard
0.47 fraction is required for a like-for-like benchmark. After conversion the two
products agree to within about 13 percent in dense forest, which confirms the
fraction; they diverge more in sparse, low-biomass areas, as expected for
independent biomass products, and that residual disagreement is used as one
estimate of observation uncertainty.

## Dependencies

The converter needs `rasterio` and `xarray`; the scoring module needs `fiona`,
`rasterio`, and `xarray`; both need the calibration module `ensemble_calibration.py`
(from the calibration additions), expected alongside these files. The tests need
only NumPy and the calibration module.

The paths in `score_esacci.py` reflect one particular setup and are illustrative;
adapt them to your own data.

## Testing

```
python test_esacci.py
```

## Figures

`esacci_figures.py` produces three figures (written to `figures/`):

- `fig_obs_error.png`: 90% coverage and spread-skill ratio, raw versus with
  observation error added. Coverage stays far below 0.9 and the ratio near its raw
  value even when the observation error is estimated generously from the
  difference between the XuSaatchi and ESACCI products.
- `fig_three_benchmarks.png`: calibration against XuSaatchi, ESACCI 2020, and
  ESACCI 2024, showing the ensemble is equally overconfident against all three.
- `fig_benchmark_comparison.png`: ESACCI carbon density versus XuSaatchi per grid
  cell, with the 1:1 line; agreement tightens toward dense forest, confirming the
  0.47 carbon conversion.
