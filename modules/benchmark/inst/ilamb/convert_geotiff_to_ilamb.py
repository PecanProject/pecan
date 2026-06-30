#!/usr/bin/env python3
"""
convert_geotiff_to_ilamb.py

Convert PEcAn SDA carbon reanalysis outputs (downscaled GeoTIFF ensemble maps)
into ILAMB-compatible CF-convention netCDF files.

The PEcAn North American carbon reanalysis (Zhang et al. 2026) provides
downscaled 1 km ensemble maps of four state variables. This tool:

  1. Reads the 100 GeoTIFF ensemble members for each variable/year
  2. Computes the ensemble mean (memory-safe, one member at a time)
  3. Coarsens from 1 km to ILAMB's default 0.5 degree resolution
  4. Applies unit conversions to CF/CMOR standards
  5. Writes CF-1.8 compliant netCDF that ILAMB can ingest directly

Input GeoTIFF layout:
    <input_dir>/<year>/<variable>_<year>/ensemble_<n>_<year>_<variable>.tiff

Output netCDF layout:
    <output_dir>/<cmor_name>/<cmor_name>_<year>.nc   (annual files)
    <output_dir>/<cmor_name>.nc                       (merged multi-year)

Usage:
    python convert_geotiff_to_ilamb.py --input_dir DIR --output_dir DIR
    python convert_geotiff_to_ilamb.py --variables AbvGrndWood --years 2014 2014

Author: Tejas Dahiya (Google Summer of Code 2026, PEcAn Project)
"""

import argparse
import glob
import os
import re
from datetime import datetime

import numpy as np
import rasterio
import xarray as xr


# -----------------------------------------------------------------------------
# Configuration
# -----------------------------------------------------------------------------

# Maps each PEcAn GeoTIFF variable to its CMOR name, target units, the
# multiplicative scale factor needed to reach those units, and a descriptive
# long name. Unit conversions are documented in the README.
#
#   AbvGrndWood : Mg C ha-1 -> kg m-2     requires x 0.1
#   TotSoilCarb : kg C m-2  -> kg m-2     no conversion
#   SoilMoistFrac: vol. %   -> kg m-2     requires x 9.98
#                  (percent / 100 x 1.0 m depth x 998 kg/m3 water density)
#   LAI         : m2 m-2    -> m2 m-2     no conversion
VARIABLE_MAP = {
    "AbvGrndWood":   {"cmor": "cVeg",  "units": "kg m-2", "scale": 0.1,
                      "long_name": "Carbon Mass in Vegetation"},
    "TotSoilCarb":   {"cmor": "cSoil", "units": "kg m-2", "scale": 1.0,
                      "long_name": "Carbon Mass in Soil Pool"},
    "SoilMoistFrac": {"cmor": "mrsol", "units": "kg m-2", "scale": 9.98,
                      "long_name": "Total Soil Moisture Content"},
    "LAI":           {"cmor": "lai",   "units": "m2 m-2", "scale": 1.0,
                      "long_name": "Leaf Area Index"},
}

# Coarsening factor: native 0.00833 deg (1 km) x 60 = 0.5 deg (ILAMB default).
# The native grid (9360 x 19080) divides evenly: 156 x 318.
COARSEN_FACTOR = 60

# GeoTIFF NoData sentinel per the ORNL DAAC documentation. Files on the SCC
# use NaN, but we guard against -9999 as well for robustness.
NODATA_VALUE = -9999


# -----------------------------------------------------------------------------
# Core processing
# -----------------------------------------------------------------------------

def process_variable_year(base_dir, year, pecan_var, output_dir):
    """Convert one variable for one year to an ILAMB-compatible netCDF.

    Reads all ensemble GeoTIFFs, computes the ensemble mean one member at a
    time (to stay within memory limits), applies the unit conversion, coarsens
    to 0.5 degrees, orients latitude south-to-north, and writes a CF-1.8 file.

    Parameters
    ----------
    base_dir : str
        Root directory of the GeoTIFF ensemble maps.
    year : int
        Year to process (e.g. 2014).
    pecan_var : str
        PEcAn variable name (key in VARIABLE_MAP).
    output_dir : str
        Directory to write the netCDF output.
    """
    info = VARIABLE_MAP[pecan_var]
    cmor_name = info["cmor"]
    var_dir = os.path.join(base_dir, str(year), f"{pecan_var}_{year}")

    if not os.path.isdir(var_dir):
        print(f"  WARNING: {var_dir} not found, skipping")
        return

    # The variable folder also contains precomputed mean and std maps
    # (mean_<year>_<var>.tiff, std_<year>_<var>.tiff). Match only the
    # numbered ensemble members so those summary maps are not averaged in.
    files = sorted(
        os.path.join(var_dir, f)
        for f in os.listdir(var_dir)
        if f.endswith((".tiff", ".tif")) and re.match(r"ensemble_\d+_", f)
    )
    n_ens = len(files)
    print(f"  {pecan_var} -> {cmor_name}: {n_ens} ensemble files")

    # Grid metadata from the first member (all members share the same grid).
    with rasterio.open(files[0]) as src:
        shape = src.shape
        bounds = src.bounds

    # Accumulate the ensemble mean with a running sum / count so we never hold
    # more than one full-resolution member in memory at once.
    running_sum = np.zeros(shape, dtype=np.float64)
    running_count = np.zeros(shape, dtype=np.float64)

    for i, f in enumerate(files):
        with rasterio.open(f) as src:
            data = src.read(1).astype(np.float64)
        data = np.where(data == NODATA_VALUE, np.nan, data)
        valid = np.isfinite(data)
        running_sum[valid] += data[valid]
        running_count[valid] += 1
        if (i + 1) % 25 == 0:
            print(f"    Read {i + 1}/{n_ens} members")

    ens_mean = np.where(running_count > 0, running_sum / running_count, np.nan)

    # Apply unit conversion to reach CMOR units.
    ens_mean *= info["scale"]

    valid_vals = ens_mean[np.isfinite(ens_mean)]
    print(f"    Mean range: {valid_vals.min():.3f} to "
          f"{valid_vals.max():.3f} {info['units']}")

    # Coarsen 1 km -> 0.5 deg by block-averaging COARSEN_FACTOR^2 pixels,
    # ignoring NaN (ocean / non-land) within each block.
    nr = shape[0] // COARSEN_FACTOR
    nc = shape[1] // COARSEN_FACTOR
    trimmed = ens_mean[:nr * COARSEN_FACTOR, :nc * COARSEN_FACTOR]
    reshaped = trimmed.reshape(nr, COARSEN_FACTOR, nc, COARSEN_FACTOR)
    with np.errstate(all="ignore"):
        coarsened = np.nanmean(reshaped, axis=(1, 3))

    # GeoTIFF rows run north-to-south; flip so latitude increases south-to-north
    # (the convention expected by ILAMB and most CF tools). Coordinates are
    # rounded to remove sub-millidegree floating point noise from the bounds.
    coarsened = np.flip(coarsened, axis=0)
    lat = np.round(
        np.flip(np.linspace(bounds.top - 0.25, bounds.bottom + 0.25, nr)), 2)
    lon = np.round(
        np.linspace(bounds.left + 0.25, bounds.right - 0.25, nc), 2)

    # Time: annual snapshot fixed to July 15 (per ORNL documentation), encoded
    # as days since 1850-01-01 with full-year bounds.
    time_val = (datetime(year, 7, 15) - datetime(1850, 1, 1)).days
    tb_start = (datetime(year, 1, 1) - datetime(1850, 1, 1)).days
    tb_end = (datetime(year, 12, 31) - datetime(1850, 1, 1)).days

    ds = xr.Dataset(
        {
            cmor_name: xr.DataArray(
                data=coarsened[np.newaxis, :, :].astype(np.float32),
                dims=["time", "lat", "lon"],
                attrs={"units": info["units"], "long_name": info["long_name"]},
            ),
            "time_bounds": xr.DataArray(
                data=np.array([[tb_start, tb_end]], dtype=np.float64),
                dims=["time", "nb"],
            ),
        },
        coords={
            "time": ("time", [time_val], {
                "units": "days since 1850-01-01 00:00:00",
                "calendar": "standard",
                "bounds": "time_bounds",
            }),
            "lat": ("lat", lat, {"units": "degrees_north"}),
            "lon": ("lon", lon, {"units": "degrees_east"}),
        },
        attrs={
            "Conventions": "CF-1.8",
            "source": ("PEcAn SDA Reanalysis (Zhang et al. 2026)"),
        },
    )

    out_var_dir = os.path.join(output_dir, cmor_name)
    os.makedirs(out_var_dir, exist_ok=True)
    outpath = os.path.join(out_var_dir, f"{cmor_name}_{year}.nc")
    ds.to_netcdf(outpath, encoding={cmor_name: {"zlib": True, "complevel": 4}})
    print(f"    Saved: {outpath}")


def merge_annual_files(output_dir):
    """Merge per-year files into one multi-year netCDF per variable."""
    for info in VARIABLE_MAP.values():
        cmor_name = info["cmor"]
        var_dir = os.path.join(output_dir, cmor_name)
        if not os.path.isdir(var_dir):
            continue
        files = sorted(glob.glob(os.path.join(var_dir, f"{cmor_name}_????.nc")))
        if len(files) < 2:
            continue
        print(f"\nMerging {len(files)} files for {cmor_name}...")
        ds = xr.open_mfdataset(files, combine="by_coords")
        merged_path = os.path.join(output_dir, f"{cmor_name}.nc")
        ds.to_netcdf(merged_path,
                     encoding={cmor_name: {"zlib": True, "complevel": 4}})
        print(f"  Saved: {merged_path}")
        ds.close()


# -----------------------------------------------------------------------------
# Command-line interface
# -----------------------------------------------------------------------------

def main():
    parser = argparse.ArgumentParser(
        description="Convert PEcAn SDA GeoTIFF outputs to ILAMB netCDF")
    parser.add_argument(
        "--input_dir",
        default=("/projectnb/dietzelab/dongchen/anchorSites/NA_runs/"
                 "SDA_8k_site/NA_SDA_maps_zipped"),
        help="Root directory of GeoTIFF ensemble maps")
    parser.add_argument(
        "--output_dir",
        default="/projectnb/dietzelab/tdahiya/ilamb_outputs/pecan_v2",
        help="Directory to write ILAMB-compatible netCDF")
    parser.add_argument(
        "--variables", nargs="+", default=None,
        help="PEcAn variable names to convert (default: all four)")
    parser.add_argument(
        "--years", nargs=2, type=int, default=None, metavar=("START", "END"),
        help="Inclusive year range to process (default: 2012 2024)")
    parser.add_argument(
        "--skip-merge", action="store_true",
        help="Skip merging annual files into multi-year files")
    args = parser.parse_args()

    variables = args.variables or list(VARIABLE_MAP.keys())
    years = (range(args.years[0], args.years[1] + 1)
             if args.years else range(2012, 2025))

    print("GeoTIFF -> ILAMB Pipeline")
    print(f"  Input:     {args.input_dir}")
    print(f"  Output:    {args.output_dir}")
    print(f"  Variables: {variables}")
    print(f"  Years:     {list(years)}")

    os.makedirs(args.output_dir, exist_ok=True)

    for year in years:
        print(f"\n=== Year {year} ===")
        for var in variables:
            process_variable_year(args.input_dir, year, var, args.output_dir)

    if not args.skip_merge:
        merge_annual_files(args.output_dir)

    print("\nDone!")


if __name__ == "__main__":
    main()
