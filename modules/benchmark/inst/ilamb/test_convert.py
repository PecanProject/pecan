"""
test_convert.py

Unit tests for convert_geotiff_to_ilamb.py.

Validates the converted ILAMB-compatible netCDF outputs: file existence,
CMOR variable naming, unit conversions, output grid shape, CF-1.8 compliance,
latitude orientation, spatial coverage, multi-year merging, and that ILAMB's
ModelResult can load the results.

Run:
    module load python3 gcc/13.2.0
    export PATH=$HOME/.local/bin:$PATH
    pytest test_convert.py -v

Author: Tejas Dahiya (Google Summer of Code 2026, PEcAn Project)
"""

import os

import numpy as np
import xarray as xr

# Directory holding the converted outputs. Override with the ILAMB_OUTPUT_DIR
# environment variable if your outputs live elsewhere.
OUTPUT_DIR = os.environ.get(
    "ILAMB_OUTPUT_DIR",
    "/projectnb/dietzelab/tdahiya/ilamb_outputs/pecan_v2",
)

CMOR_VARS = ["cVeg", "cSoil", "mrsol", "lai"]
YEARS = range(2012, 2025)


def test_all_files_exist():
    """Every variable has 13 annual files plus a merged multi-year file."""
    for var in CMOR_VARS:
        merged = os.path.join(OUTPUT_DIR, f"{var}.nc")
        assert os.path.exists(merged), f"Missing merged file: {merged}"
        for year in YEARS:
            annual = os.path.join(OUTPUT_DIR, var, f"{var}_{year}.nc")
            assert os.path.exists(annual), f"Missing annual file: {annual}"


def test_variable_names():
    """Each output uses the correct CMOR variable name."""
    for cmor_name in CMOR_VARS:
        ds = xr.open_dataset(
            os.path.join(OUTPUT_DIR, cmor_name, f"{cmor_name}_2014.nc"))
        assert cmor_name in ds.data_vars, f"{cmor_name} not in dataset"
        ds.close()


def test_output_shape():
    """Coarsened grid is the expected 0.5 degree resolution (156 x 318)."""
    ds = xr.open_dataset(os.path.join(OUTPUT_DIR, "cVeg", "cVeg_2014.nc"))
    assert ds.sizes["time"] == 1
    assert ds.sizes["lat"] == 156
    assert ds.sizes["lon"] == 318
    ds.close()


def test_cveg_unit_conversion():
    """cVeg (AGB) is converted from Mg C ha-1 to kg m-2 (x 0.1)."""
    ds = xr.open_dataset(os.path.join(OUTPUT_DIR, "cVeg", "cVeg_2014.nc"))
    valid = ds["cVeg"].values[np.isfinite(ds["cVeg"].values)]
    assert valid.min() >= 0, "cVeg has negative values"
    assert valid.max() < 60, f"cVeg max {valid.max()} too high (units?)"
    assert valid.mean() > 0.1, "cVeg mean suspiciously low"
    assert ds["cVeg"].attrs["units"] == "kg m-2"
    ds.close()


def test_csoil_values():
    """cSoil (SOC) stays in kg m-2 with a physically reasonable range."""
    ds = xr.open_dataset(os.path.join(OUTPUT_DIR, "cSoil", "cSoil_2014.nc"))
    valid = ds["cSoil"].values[np.isfinite(ds["cSoil"].values)]
    assert valid.min() > 0, "cSoil has zero/negative values"
    assert valid.max() < 300, f"cSoil max {valid.max()} unreasonably high"
    assert ds["cSoil"].attrs["units"] == "kg m-2"
    ds.close()


def test_lai_values():
    """lai stays in m2 m-2 with a physically reasonable range."""
    ds = xr.open_dataset(os.path.join(OUTPUT_DIR, "lai", "lai_2014.nc"))
    valid = ds["lai"].values[np.isfinite(ds["lai"].values)]
    assert valid.min() >= 0, "LAI has negative values"
    assert valid.max() < 15, f"LAI max {valid.max()} unreasonably high"
    assert ds["lai"].attrs["units"] == "m2 m-2"
    ds.close()


def test_mrsol_values():
    """mrsol is converted from volumetric percent to kg m-2 (x 9.98)."""
    ds = xr.open_dataset(os.path.join(OUTPUT_DIR, "mrsol", "mrsol_2014.nc"))
    valid = ds["mrsol"].values[np.isfinite(ds["mrsol"].values)]
    assert valid.min() >= 0, "mrsol has negative values"
    assert valid.max() < 600, f"mrsol max {valid.max()} too high for kg m-2"
    assert valid.mean() > 50, f"mrsol mean {valid.mean()} too low for kg m-2"
    assert ds["mrsol"].attrs["units"] == "kg m-2"
    ds.close()


def test_cf_time_encoding():
    """Time is CF-encoded as days since 1850-01-01 with bounds."""
    ds = xr.open_dataset(
        os.path.join(OUTPUT_DIR, "cVeg", "cVeg_2014.nc"), decode_times=False)
    assert "time" in ds.coords
    assert ds.time.attrs.get("units") == "days since 1850-01-01 00:00:00"
    assert ds.time.attrs.get("calendar") == "standard"
    assert "time_bounds" in ds.data_vars
    ds.close()


def test_cf_coordinates():
    """Coordinate attributes and global Conventions are CF-compliant."""
    ds = xr.open_dataset(os.path.join(OUTPUT_DIR, "cVeg", "cVeg_2014.nc"))
    assert ds.lat.attrs.get("units") == "degrees_north"
    assert ds.lon.attrs.get("units") == "degrees_east"
    assert ds.attrs.get("Conventions") == "CF-1.8"
    ds.close()


def test_lat_increasing():
    """Latitude is monotonically increasing (south to north)."""
    ds = xr.open_dataset(os.path.join(OUTPUT_DIR, "cVeg", "cVeg_2014.nc"))
    assert ds.lat.values[1] > ds.lat.values[0], "Latitude not increasing"
    assert ds.lat.values[0] == 7.25, f"First lat {ds.lat.values[0]} != 7.25"
    assert ds.lat.values[-1] == 84.75, f"Last lat {ds.lat.values[-1]} != 84.75"
    ds.close()


def test_spatial_coverage():
    """Grid covers the North American study area (7-85N, 179-20W)."""
    ds = xr.open_dataset(os.path.join(OUTPUT_DIR, "cVeg", "cVeg_2014.nc"))
    assert float(ds.lat.min()) < 10, "Southern boundary too far north"
    assert float(ds.lat.max()) > 80, "Northern boundary too far south"
    assert float(ds.lon.min()) < -170, "Western boundary too far east"
    assert float(ds.lon.max()) > -25, "Eastern boundary too far west"
    ds.close()


def test_merged_file_years():
    """Merged file spans all 13 years in chronological order."""
    ds = xr.open_dataset(
        os.path.join(OUTPUT_DIR, "cVeg.nc"), decode_times=False)
    assert ds.sizes["time"] == 13, f"Expected 13 years, got {ds.sizes['time']}"
    times = ds.time.values
    assert all(times[i] < times[i + 1] for i in range(len(times) - 1)), \
        "Merged time axis is not monotonically increasing"
    ds.close()


def test_ilamb_loads():
    """ILAMB's ModelResult can load all four converted variables."""
    from ILAMB.ModelResult import ModelResult
    m = ModelResult(OUTPUT_DIR, modelname="PEcAn")
    assert m.name == "PEcAn"
    expected = set(CMOR_VARS)
    found = set(m.variables.keys()) & expected
    assert found == expected, f"Missing variables: {expected - found}"
