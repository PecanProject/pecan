# HLS

Shared helpers for NASA Harmonized Landsat Sentinel-2 (HLS) used by both the phenology product and the tillage index. HLS scenes are indexed by the Military Grid Reference System (MGRS). Intermediate files live under `$HLS_ROOT`. Inventory products live under `$PRODUCTS_INVENTORY`.

Two parcel-level extracts reuse `parcel_tiles.csv` and `R/tilewise_core.R`: annual MSLSP ([phenology](../phenology/README.md)) and monthly NDTI ([tillage](../tillage/README.md)). Commands: [Session 2](../documentation/sessions/02-phenology.md).

Do not edit the clones. Earthdata download wraps the HLS_Phenology clone `download_updated.R`. Convert is our `convert_hls_scenes.R` (not that clone's `conversion.R`). Tile phenology is the [MSLSP](https://www.earthdata.nasa.gov/data/catalog/lpcloud-mslsp30na-011) algorithm at `$MSLSP_ALGO_ROOT` on HLS v2 GeoTIFF.

## Layout

```
hls/
  build_hls_parcel_tile_map.R
  download_hls_earthdata.sh / .R
  convert_hls_scenes.R
  run_mslsp_tile.sh / .R
  R/parcel_tilemap.R
  R/tilewise_core.R
```

## Assumptions

The download window is `$PRIOR_YEAR` / `$TARGET_YEAR` plus `HLS_DOWNLOAD_BUFFER_DAYS` (default 185), Sentinel-2 (HLSS30) then Landsat (HLSL30) unless `HLS_DOWNLOAD_DOI` is set. `HLS_DOWNLOAD_TILE` / `HLS_CONVERSION_TILE` restrict to one MGRS tile; unset is California. Water mask, DEM, slope, and aspect are required for topographic correction in the MSLSP algorithm. `parcel_tiles.csv` intersects all field polygons in `parcels-consolidated.gpkg` with the California MGRS grid; year-specific crop identity is applied later at extract. Rebuild the CSV when the gpkg is rebuilt. Do not nest prep files under a tile directory. Do not rebuild `s2_mgrs_grid_ca.gpkg` from imagery (California subset of the [Zenodo Sentinel-2 tiling grid](https://zenodo.org/records/10998972), filtered to `tileids.txt`).

The wrapper does not edit the MSLSP clone. It writes a per-tile JSON and, for the HLS v2 `_dev` driver, a patched copy under the tile output dir (allocate `imgName_strip`, subset with `[keep]`, pass scene directories into QA / topo). The HLS v1 `MSLSP_Script.r` path still opens Fmask as HDF.

## Steps

### Download and convert

Download HLS surface reflectance into `$HLS_DOWNLOAD_OUTDIR`. Convert moves scratch GeoTIFFs into `$HLS_IMAGERY_ROOT/<tile>/images/` and copies water / DEM / slope / aspect for the tile.

### Parcel x tile map

Reproject the static MGRS grid to the LandIQ parcels CRS, then intersect all parcels. Output: `$HLS_ROOT/parcel_tiles.csv` (`parcel_id`, `tile_id`). Pass `overwrite` on the script to replace an existing CSV.

### MSLSP on one tile

One cluster job = one MGRS tile. For years outside the NASA MSLSP30NA archive, this runs the HLS phenology algorithm on HLSL30/HLSS30 surface reflectance (Blue, Red, NIR, Fmask, plus the ancillary water / DEM / slope / aspect layers). Hours, not minutes.

1. **QA mask.** Fmask cloud, cloud shadow, adjacent cloud, cirrus, and snow, plus the static water mask, are set aside. Negative reflectance and any pixel missing a required band become NA. Snow is retained as a fill so dormant timing can still use it. The tile is split into `HLS_MSLSP_NUM_CHUNKS` (default 196) pixel chunks; each scene is written under `$MSLSP_NETCDF_ROOT/<tile>/imageChunks/`. Empty chunks are skipped.

2. **Topographic correction.** Per calendar year: NDVI / NBR quantiles, k-means terrain classes, then reflectance correction by class using slope and aspect (radians * 10000, as shipped in the ancillary GeoTIFFs).

3. **EVI2 spline.** Each chunk builds an EVI2 time series from masked, topo-corrected HLS, despikes, and splines. Up to two phenological cycles per year (cycle 1 = largest EVI2 amplitude). Thresholds on that spline are the product dates (same names as [MSLSP30NA](https://www.earthdata.nasa.gov/data/catalog/lpcloud-mslsp30na-011)):

| Metric | Meaning | Represents |
|--------|---------|------------|
| OGI | Onset Greenness Increase (15% greenness increase) | Planting |
| 50PCGI | 50 Percent Greenness Increase | Phenology leaf-on (hay, woody) |
| Peak | Date of cycle peak | Match (which cycle) |
| OGD | Onset Greenness Decrease (10% greenness decrease) | Harvest (hay, woody) |
| 50PCGD | 50 Percent Greenness Decrease | Phenology leaf-off (hay, woody) |
| OGMn | Onset Greenness Minimum (85% greenness decrease) | Harvest (row, rice); tillage fallow |

Output: `$MSLSP_NETCDF_ROOT/<tile>/phenoMetrics/MSLSP_<tile>_<year>.nc` for `$PRIOR_YEAR` and `$TARGET_YEAR`, plus `_Extended_QA.nc`. Parcel extract does not re-run this.

There is a lag between actual planting and the first detection of foliage. Temporal resolution is limited by clear-day overpass frequency.

## Parameters and flags

| Name | Default | Role |
|------|---------|------|
| `HLS_DOWNLOAD_TILE` | unset (CA bbox) | One MGRS tile |
| `HLS_DOWNLOAD_BUFFER_DAYS` | `185` | Days before PRIOR Jan 1 / after TARGET Dec 31 |
| `HLS_DOWNLOAD_FROM` / `HLS_DOWNLOAD_TO` | derived | Explicit date window |
| `HLS_DOWNLOAD_DOI` | unset (S30 then L30) | Single collection |
| `HLS_DOWNLOAD_NCORE` | job CPUs or 16 | Download parallelism |
| `HLS_CONVERSION_TILE` | unset | Restrict convert + ancillary copy |
| `HLS_CONVERSION_OVERWRITE` | false | Replace existing scene dirs |
| `HLS_CONVERSION_COPY` | false | Copy instead of move from scratch |
| `HLS_MSLSP_NCORES` | job CPUs or 8 | MSLSP workers |
| `HLS_MSLSP_NUM_CHUNKS` | `196` | Pixel chunks (30 m operational) |
| `HLS_MSLSP_DRY_RUN` | false | JSON + dirs only |

| Path | Role |
|------|------|
| `$HLS_PHENOLOGY_ROOT/tileids.txt` | CA tile IDs (`$MSLSP_TILE_LIST`) |
| `$HLS_S2_MGRS_GRID` | `s2_mgrs_grid_ca.gpkg` |
| `$HLS_ROOT/parcel_tiles.csv` | All parcels x tile (`$HLS_PARCEL_TILES_DIR`) |
| `$HLS_IMAGERY_ROOT` | Converted scenes |
| `$MSLSP_NETCDF_ROOT` | Tile NetCDF |
