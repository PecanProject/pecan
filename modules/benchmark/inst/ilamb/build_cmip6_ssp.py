"""
Build a continuous CMIP6 record by splicing historical and ssp245 runs.

Same models, variables, and regridding as the historical builder, but joins the
end of each model's historical run to the start of its ssp245 scenario so the
record extends past 2014. Only models that provide both pieces with land-carbon
output are kept; the rest are reported as having no usable scenario run.

This spliced record is the source for the longer evaluation window, where
interannual variability and trends can be assessed. LAI is again reduced to July.

Output: one directory per model with cVeg.nc / cSoil.nc / lai.nc.

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
ap.add_argument("--outroot", default="/projectnb/dietzelab/tdahiya/ilamb_models_2012_2023")
ap.add_argument("--pecan-grid", dest="pec",
                default="/projectnb/dietzelab/tdahiya/ilamb_models/PEcAn/cVeg.nc",
                help="A PEcAn netCDF whose lat/lon define the target grid.")
args = ap.parse_args()
CACHE, OUTROOT, PEC = args.cache, args.outroot, args.pec

intake_esgf.conf.set(local_cache=[CACHE])

print("=== Download ssp245 (historical already cached) ===", flush=True)
cat = ESGFCatalog()
cat.search(project="CMIP6", experiment_id="ssp245", source_id=MODELS,
           variable_id=["cVeg","cSoil","lai"], table_id=["Lmon","Emon"], variant_label="r1i1p1f1")
cat.to_path_dict()
print("Download done.\n=== Splice hist(2012-2014)+ssp245(2015-2023) ===", flush=True)

pec = xr.open_dataset(PEC, decode_times=False)
tgt_lat, tgt_lon = pec.lat.values, pec.lon.values

def load_exp(model, var, exp, t0, t1):
    files = sorted(glob.glob(f"{CACHE}/**/{model}/**/{var}_*.nc", recursive=True))
    files = [f for f in files if exp in os.path.basename(f)]
    if not files: return None
    da = xr.open_mfdataset(files, decode_times=True, combine="by_coords")[var]
    return da.sel(time=slice(t0, t1))

def proc(model, var):
    hist = load_exp(model, var, "historical", "2012-01-01", "2014-12-31")
    ssp  = load_exp(model, var, "ssp245",     "2015-01-01", "2023-12-31")
    if hist is None or ssp is None: return None
    units = hist.attrs.get("units","")
    da = xr.concat([hist, ssp], dim="time").sortby("time")
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

ok=[]; no_ssp=[]
for m in MODELS:
    try:
        od=f"{OUTROOT}/{m}"; os.makedirs(od, exist_ok=True); n=0
        for var in ["cVeg","cSoil","lai"]:
            da=proc(m,var)
            if da is not None:
                xr.Dataset({var:da}).to_netcdf(f"{od}/{var}.nc"); n+=1
        print(f"  {m}: {'OK' if n==3 else 'PARTIAL'} ({n}/3)", flush=True)
        if n==3: ok.append(m)
        elif n==0: no_ssp.append(m)
    except Exception as e:
        print(f"  {m}: FAILED - {str(e)[:90]}", flush=True)
print(f"\nDone. {len(ok)}/{len(MODELS)} full splice: {ok}", flush=True)
print(f"No usable ssp245 (likely no scenario run): {no_ssp}", flush=True)
