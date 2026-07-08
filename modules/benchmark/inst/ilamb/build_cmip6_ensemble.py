"""
Download and regrid CMIP6 historical land-carbon fields for benchmarking.

Retrieves cVeg, cSoil, and lai for a set of CMIP6 models (one realization each,
r1i1p1f1) from the ESGF archive via intake-esgf, then regrids every field onto
the PEcAn analysis grid so all models, observations, and the reanalysis share a
common grid for ILAMB scoring. Downloads are cached, so re-runs resume rather
than refetch.

LAI is reduced to the July value to match the growing-season convention used on
the reanalysis side; carbon pools use the full monthly series. Longitudes are
converted to the [-180, 180] convention where needed before interpolation.

Output: one directory per model, each containing cVeg.nc / cSoil.nc / lai.nc.
The full historical span is written here; restriction to an evaluation window
is handled downstream by build_window_ensembles.py.

Paths default to the values used for this analysis; override with the flags
below to run elsewhere.
"""
import argparse
import os
import glob
import xarray as xr
import intake_esgf
from intake_esgf import ESGFCatalog

MODELS = ["ACCESS-ESM1-5","AWI-ESM-1-1-LR","BCC-CSM2-MR","BCC-ESM1","CESM2",
          "CESM2-FV2","CESM2-WACCM","CESM2-WACCM-FV2","CMCC-CM2-SR5","CMCC-ESM2",
          "CanESM5","CanESM5-1","EC-Earth3-CC","EC-Earth3-Veg","EC-Earth3-Veg-LR",
          "GFDL-ESM4","IPSL-CM5A2-INCA","IPSL-CM6A-LR","IPSL-CM6A-LR-INCA","KIOST-ESM",
          "MPI-ESM-1-2-HAM","MPI-ESM1-2-LR","NorESM2-LM","NorESM2-MM","SAM0-UNICON","TaiESM1"]

ap = argparse.ArgumentParser(description=__doc__)
ap.add_argument("--cache",   default="/projectnb/dietzelab/tdahiya/cmip6_cache")
ap.add_argument("--outroot", default="/projectnb/dietzelab/tdahiya/ilamb_models_multi")
ap.add_argument("--pecan-grid", dest="pec",
                default="/projectnb/dietzelab/tdahiya/ilamb_models/PEcAn/cVeg.nc",
                help="A PEcAn netCDF whose lat/lon define the target grid.")
args = ap.parse_args()
CACHE, OUTROOT, PEC = args.cache, args.outroot, args.pec

intake_esgf.conf.set(local_cache=[CACHE])

print("=== Download (resumes from cache) ===", flush=True)
cat = ESGFCatalog()
cat.search(project="CMIP6", experiment_id="historical", source_id=MODELS,
           variable_id=["cVeg","cSoil","lai"], table_id=["Lmon","Emon"], variant_label="r1i1p1f1")
cat.to_path_dict()
print("Download done.\n=== Processing ===", flush=True)

pec = xr.open_dataset(PEC, decode_times=False)
tgt_lat, tgt_lon = pec.lat.values, pec.lon.values

def proc(model, var):
    files = sorted(glob.glob(f"{CACHE}/**/{model}/**/{var}_*.nc", recursive=True))
    if not files: return None
    da = xr.open_mfdataset(files, decode_times=True, combine="by_coords")[var]
    units = da.attrs.get("units","")
    da = da.sel(time=slice("2000-01-01","2014-12-31"))
    if var == "lai":
        da = da.sel(time=da.time.dt.month == 7)
    if float(da.lon.max()) > 180 and tgt_lon.min() < 0:
        da = da.assign_coords(lon=(((da.lon+180)%360)-180)).sortby("lon")
    da = da.interp(lat=tgt_lat, lon=tgt_lon, method="linear").load()
    da.attrs["units"]=units; da.name=var
    da["time"].attrs.pop("bounds",None); da["time"].encoding.pop("bounds",None)
    da["lat"].attrs.update({"units":"degrees_north","standard_name":"latitude"})
    da["lon"].attrs.update({"units":"degrees_east","standard_name":"longitude"})
    return da

ok=[]
for m in MODELS:
    try:
        od=f"{OUTROOT}/{m}"; os.makedirs(od, exist_ok=True); n=0
        for var in ["cVeg","cSoil","lai"]:
            da=proc(m,var)
            if da is not None:
                xr.Dataset({var:da}).to_netcdf(f"{od}/{var}.nc"); n+=1
        print(f"  {m}: {'OK' if n==3 else 'PARTIAL'} ({n}/3)", flush=True)
        if n==3: ok.append(m)
    except Exception as e:
        print(f"  {m}: FAILED - {str(e)[:90]}", flush=True)

print(f"\nDone. {len(ok)}/{len(MODELS)} fully processed:", ok)
