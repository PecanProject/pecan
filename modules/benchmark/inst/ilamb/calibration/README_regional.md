# Regional Calibration Diagnostics

Breaks the ensemble calibration assessment of a carbon reanalysis down by region,
rather than reporting a single domain-wide number. It answers a question the
domain-wide diagnostics cannot: does the trustworthiness of the ensemble spread
vary by biome and by ecoregion?

This builds directly on the ensemble calibration diagnostics in
`ensemble_calibration.py`, computing the same spread-to-error
ratio and coverage, but on subsets of sites grouped two ways: by land cover class
and by EPA/CEC ecoregion.

## What it does

At a set of point locations (here, the sites of a state data assimilation
reanalysis), the ensemble and an observational benchmark are sampled, and the
calibration diagnostics are computed per group:

- **`regional_diagnostics.py`** samples the per-member downscaled maps and the
  benchmark at each site and computes calibration per land cover class. Its
  `stratified_calibration(members, obs, groups, labels)` engine is grouping
  agnostic and is reused for the ecoregion breakdown.
- **`ecoregion_join.py`** assigns each site an EPA/CEC North American ecoregion
  (Level 1 and Level 2) by point-in-polygon, then runs the same engine on the
  ecoregion grouping.
- **`regional_figures.py`** renders choropleth maps of the ecoregions shaded by
  calibration, bar charts by land cover and by ecoregion, and a map of the sites
  coloured by land cover class.
- **`regional_diagnostics.ipynb`** ties these together into a narrative with the
  tables, figures, and interpretation.

## Interpretation

The metrics are the same as in the calibration module: a spread-to-error ratio
near 1 indicates a well-sized ensemble, well below 1 indicates overconfidence,
and coverage is the fraction of observations inside the ensemble 90 percent band
(near 0.90 when calibrated). Reporting these per region reveals structure that
the single domain-wide number hides, for example whether the ensemble is
overconfident uniformly or only in particular biomes.

## Dependencies and data

The stratification engine is pure NumPy plus the calibration module. The
ecoregion join additionally requires `geopandas` and the figures additionally
require `matplotlib`.

The ecoregion breakdown needs the CEC "Ecoregions of North America" Level 1 and
Level 2 shapefiles, a free download from the EPA
(https://www.epa.gov/eco-research/ecoregions-north-america, files
`na_cec_eco_l1.zip` and `na_cec_eco_l2.zip`). Use the full North American files
rather than the CONUS-only versions bundled with some tools, so that the boreal
and Arctic north is covered. Unzip them and point `ECO_DIR` in
`ecoregion_join.py` at the folder containing `NA_CEC_Eco_Level1.shp` and
`NA_CEC_Eco_Level2.shp`.

## Notes

The paths in these modules (the SDA output directory, the downscaled-map layout,
the benchmark locations, and `ECO_DIR`) reflect one particular setup and are
illustrative; adapt them to your own data. The land cover class labels in
`LANDCOVER_LABELS` are inferred from the geography and the biomass and LAI
signature of each class and are intended to be replaced with the confirmed
scheme; only these display strings change, not the computed numbers.

## Testing

`test_regional_diagnostics.py` checks the `stratified_calibration` engine on
synthetic data with known per-group calibration, without touching any real data
files. Run with:

```
python test_regional_diagnostics.py
```
