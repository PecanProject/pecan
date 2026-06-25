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
