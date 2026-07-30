"""
Download and regrid TRENDY (Global Carbon Budget) model fields for benchmarking.

Reads the published TRENDY file index, resolves the public download URL for each
model's cVeg / cSoil / lai output, and regrids every field onto the PEcAn
analysis grid. The index is the source of truth for filenames because a model's
directory name does not always match its file-name prefix, so URLs are taken
from the index rather than reconstructed.

TRENDY files vary in how they encode time (annual vs monthly, and several
different time-unit conventions); the time decoder handles these cases and
derives calendar years and months consistently. Coordinates are normalised to
lat/lon, longitudes shifted to [-180, 180] where needed, and LAI reduced to July.

Output: one directory per model (prefixed "TRENDY-"), each with the available
cVeg.nc / cSoil.nc / lai.nc.

Paths default to the values used for this analysis; override with the flags
below to run elsewhere.
"""
import argparse
import xarray as xr
import numpy as np
import pandas as pd
import re
import os
import json
import urllib.request
import subprocess

BASE = "https://s3.eu-west-1.wasabisys.com/gcb-2024-trendy"
INDEX_URL = "https://raw.githubusercontent.com/mdosullivan/GCB/main/fileIndex_merged_v4.json"

ap = argparse.ArgumentParser(description=__doc__)
ap.add_argument("--dldir",   default="/projectnb/dietzelab/tdahiya/trendy_cache")
ap.add_argument("--outroot", default="/projectnb/dietzelab/tdahiya/ilamb_models_multi")
ap.add_argument("--pecan-grid", dest="pec",
                default="/projectnb/dietzelab/tdahiya/ilamb_models/PEcAn/cVeg.nc",
                help="A PEcAn netCDF whose lat/lon define the target grid.")
args = ap.parse_args()
DLDIR, OUTROOT, PEC = args.dldir, args.outroot, args.pec
os.makedirs(DLDIR, exist_ok=True)

idx=f"{DLDIR}/fileIndex.json"
if not os.path.exists(idx):
    urllib.request.urlretrieve(INDEX_URL, idx)
paths=[e[0] for e in json.load(open(idx))]
fmap={}
for p in paths:
    mm=re.match(r"trendyv13-gcb2024/([^/]+)/S3/[^/]+_S3_(cVeg|cSoil|lai)\.nc$", p)
    if mm: fmap[(mm.group(1),mm.group(2))]=p.replace("trendyv13-gcb2024/","")
MODELS=sorted({k[0] for k in fmap})
print(f"{len(MODELS)} models in index", flush=True)

pec=xr.open_dataset(PEC, decode_times=False); tgt_lat,tgt_lon=pec.lat.values,pec.lon.values

def yrs_mons(ds):
    t=ds["time"]; u=t.attrs.get("units","") or ""; v=np.asarray(t.values); n=len(v)
    if u.startswith("years since"):
        ref=int(re.search(r"since\s+(\d+)",u).group(1)); return ref+v.astype(int), np.full(n,7)
    if u.startswith("months since"):
        ms=re.search(r"since\s+(\d+)-(\d+)",u); ry,rm=int(ms.group(1)),int(ms.group(2))
        tot=(rm-1)+v.astype(int); return ry+tot//12,(tot%12)+1
    if "year as" in u: return np.floor(v).astype(int), np.full(n,7)
    if n==324:  return 1700+np.arange(n), np.full(n,7)
    if n==3888: return 1700+np.arange(n)//12, (np.arange(n)%12)+1
    import cftime; dts=cftime.num2date(v,u); return np.array([d.year for d in dts]),np.array([d.month for d in dts])

def std(da):
    ren={c:("lat" if str(c).lower() in ("latitude","y") else "lon") for c in da.coords if str(c).lower() in ("latitude","longitude","y","x")}
    return da.rename(ren)

def proc(path, var):
    ds=xr.open_dataset(path, decode_times=False)
    vin=var if var in ds else next((x for x in ds.data_vars if x.lower()==var.lower()), None)
    if vin is None: return None
    units=ds[vin].attrs.get("units","")
    years,months=yrs_mons(ds)
    mask=(years>=2000)&(years<=2014) & ((months==7) if var=="lai" else True)
    i=np.where(mask)[0]
    if len(i)==0: return None
    da=std(ds[vin]).isel(time=i).assign_coords(time=pd.to_datetime([f"{y}-07-01" for y in years[i]]))
    if float(da.lon.max())>180 and tgt_lon.min()<0:
        da=da.assign_coords(lon=(((da.lon+180)%360)-180)).sortby("lon")
    da=da.sortby("lat")
    rg=da.interp(lat=tgt_lat,lon=tgt_lon,method="linear").load()
    rg.attrs["units"]=units; rg.name=var
    rg["time"].attrs.pop("bounds",None); rg["time"].encoding.pop("bounds",None)
    rg["lat"].attrs.update({"units":"degrees_north","standard_name":"latitude"})
    rg["lon"].attrs.update({"units":"degrees_east","standard_name":"longitude"})
    return rg

def get_var(m, var):
    if (m,var) not in fmap: return None
    f=f"{DLDIR}/{m}_S3_{var}.nc"; url=f"{BASE}/{fmap[(m,var)]}"
    for attempt in range(2):
        if not (os.path.exists(f) and os.path.getsize(f)>10000):
            subprocess.run(["wget","-q","-O",f,url])
        try:
            return proc(f, var)
        except Exception:
            if os.path.exists(f): os.remove(f)   # corrupt -> redownload once
    return None

print("=== Download + process ===", flush=True)
ok=[]
for m in MODELS:
    od=f"{OUTROOT}/TRENDY-{m}"; os.makedirs(od, exist_ok=True); n=0
    for var in ["cVeg","cSoil","lai"]:
        try:
            rg=get_var(m,var)
            if rg is not None:
                xr.Dataset({var:rg}).to_netcdf(f"{od}/{var}.nc"); n+=1
        except Exception as e:
            print(f"    {m} {var}: {str(e)[:55]}", flush=True)
    print(f"  TRENDY-{m}: ({n}/3)", flush=True)
    if n>=2: ok.append(m)
print(f"\nDone. {len(ok)} models: {ok}", flush=True)
