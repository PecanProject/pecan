"""
Build per-member PEcAn reanalysis fields for ensemble benchmarking.

The PEcAn state-data-assimilation reanalysis is an ensemble; each member is a
separate realization stored as annual GeoTIFFs. This script reads all members,
converts each to the benchmark variables on the analysis grid, and writes one
directory per member so that members can be scored individually (alongside the
individual CMIP6 and TRENDY models) to characterise ensemble spread.

Per variable: above-ground wood is converted to vegetation carbon, total soil
carbon is carried through, and LAI is carried through. Native high-resolution
rasters are block-averaged to the analysis grid; the no-data value is masked
before averaging.

Output: one directory per member (PEcAn-001 ... PEcAn-NNN), each with
cVeg.nc / cSoil.nc / lai.nc for the evaluation window.

Paths and window default to the values used for this analysis; override with
the flags below (e.g. --years 2012 2014 --outroot ilamb_models_members_2012_2014).
"""
import argparse
import os
import numpy as np
import xarray as xr
import rasterio
import pandas as pd

VMAP = {"AbvGrndWood": ("cVeg", 0.1), "TotSoilCarb": ("cSoil", 1.0), "LAI": ("lai", 1.0)}
COARSEN = 60

ap = argparse.ArgumentParser(description=__doc__)
ap.add_argument("--src",
                default="/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site/NA_SDA_maps_zipped",
                help="Root of the PEcAn SDA GeoTIFF ensemble maps.")
ap.add_argument("--outroot", default="/projectnb/dietzelab/tdahiya/ilamb_models_members",
                help="Output root; one PEcAn-NNN directory is written per member.")
ap.add_argument("--pecan-grid", dest="pec",
                default="/projectnb/dietzelab/tdahiya/ilamb_models/PEcAn/cVeg.nc",
                help="A PEcAn netCDF whose lat/lon define the target grid.")
ap.add_argument("--years", nargs=2, type=int, metavar=("START", "END"),
                default=[2015, 2023], help="Inclusive year range (default 2015 2023).")
ap.add_argument("--n-members", type=int, default=100, help="Number of members.")
args = ap.parse_args()

SRC, OUTROOT, PEC = args.src, args.outroot, args.pec
YEARS = list(range(args.years[0], args.years[1] + 1))
NMEM = args.n_members

pec = xr.open_dataset(PEC, decode_times=False)
tgt_lat = pec.lat.values
tgt_lon = pec.lon.values


def read_member(pvar, year, mem):
    d = f"{SRC}/{year}/{pvar}_{year}"
    f = f"{d}/ensemble_{mem}_{year}_{pvar}.tiff"
    if not os.path.exists(f): f = f"{d}/ensemble_{mem}_{year}_{pvar}.tif"
    if not os.path.exists(f): return None
    with rasterio.open(f) as src:
        data = src.read(1).astype(np.float64)
    data = np.where(data == -9999, np.nan, data)
    nr = data.shape[0] // COARSEN; nc = data.shape[1] // COARSEN
    t = data[:nr * COARSEN, :nc * COARSEN].reshape(nr, COARSEN, nc, COARSEN)
    with np.errstate(all="ignore"):
        c = np.nanmean(t, axis=(1, 3))
    return np.flip(c, axis=0)


print(f"=== Processing {NMEM} PEcAn members, {YEARS[0]}-{YEARS[-1]} ===", flush=True)
ok = 0
for mem in range(1, NMEM + 1):
    od = f"{OUTROOT}/PEcAn-{mem:03d}"; os.makedirs(od, exist_ok=True); nv = 0
    for pvar, (cmor, scale) in VMAP.items():
        frames = []
        for y in YEARS:
            arr = read_member(pvar, y, mem)
            if arr is None: frames = None; break
            frames.append(arr * scale)
        if frames is None: continue
        stack = np.array(frames)
        da = xr.DataArray(stack, dims=("time", "lat", "lon"),
                          coords={"time": pd.to_datetime([f"{y}-07-01" for y in YEARS]),
                                  "lat": tgt_lat, "lon": tgt_lon}, name=cmor)
        da.attrs["units"] = "kg m-2" if cmor != "lai" else "m2 m-2"
        da["lat"].attrs.update({"units": "degrees_north", "standard_name": "latitude"})
        da["lon"].attrs.update({"units": "degrees_east", "standard_name": "longitude"})
        xr.Dataset({cmor: da}).to_netcdf(f"{od}/{cmor}.nc"); nv += 1
    if nv == 3: ok += 1
    if mem % 20 == 0: print(f"  ...{mem}/{NMEM} done", flush=True)
print(f"\nDone. {ok}/{NMEM} members fully processed.", flush=True)
