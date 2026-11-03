#!/usr/bin/env python3
"""
Build per-window model ensembles for the PEcAn / CMIP6 / TRENDY benchmark.

The CMIP6 and TRENDY download/regrid scripts produce full-length, regridded
model fields on the PEcAn analysis grid. This script restricts those fields to
a chosen evaluation window and builds the ensemble means used in the ILAMB
scorecard: the CMIP6 ensemble, the TRENDY ensemble, and the PEcAn ensemble.

Two evaluation windows are supported by default:

  * 2012-2014 : a mean-state snapshot. Uses the full set of CMIP6 historical
                models, the broadest representative model sample.
  * 2015-2023 : a longer record for interannual variability and trends. Uses
                the CMIP6 models that provide a continuous historical+ssp245
                land-carbon record.

For each window the script:
  1. slices every individual CMIP6 / TRENDY model to the window years,
  2. collapses each model to annual values (July for LAI),
  3. averages across models to form the CMIP6 and TRENDY ensemble means,
  4. averages the per-member PEcAn fields to form the PEcAn ensemble mean.

The TRENDY ensemble is built only from the members listed in the screening
manifest (models that pass a per-variable physical-plausibility check), and its
fields are clipped at zero because a few models report small negative values
after regridding. The PEcAn ensemble mean is the mean of the per-member fields
produced by build_pecan_members.py.

Example:
    python build_window_ensembles.py 2012_2014 \
        --src ilamb_models_multi \
        --pecan-members-dir ilamb_models_members_2012_2014 \
        --manifest trendy_ensemble_manifest.json \
        --dst ilamb_models_2012_2014
"""

import os
import json
import glob
import argparse
import pandas as pd
import xarray as xr

VARS = ["cVeg", "cSoil", "lai"]
ENSEMBLE_DIRS = ("CMIP6-ensemble", "TRENDY-ensemble", "PEcAn")

WINDOW_DEFAULTS = {
    "2012_2014": dict(src="ilamb_models_multi",     years=(2012, 2014)),
    "2015_2023": dict(src="ilamb_models_2012_2023", years=(2015, 2023)),
}


def list_individual_models(src):
    """Return model directories under `src`, excluding the ensemble-mean dirs."""
    out = []
    for d in sorted(os.listdir(src)):
        p = os.path.join(src, d)
        if not os.path.isdir(p) or d in ENSEMBLE_DIRS:
            continue
        if not any(f.endswith(".nc") for f in os.listdir(p)):
            continue
        out.append(d)
    return out


def slice_models(src, dst, models, year0, year1):
    """Slice each model's variables to [year0, year1] and write to `dst`."""
    os.makedirs(dst, exist_ok=True)
    for m in models:
        od = os.path.join(dst, m)
        os.makedirs(od, exist_ok=True)
        for var in VARS:
            f = os.path.join(src, m, f"{var}.nc")
            if not os.path.exists(f):
                continue
            try:
                ds = xr.open_dataset(f)
                sub = ds.sel(time=slice(str(year0), str(year1)))
                if sub.sizes.get("time", 0) > 0:
                    sub.to_netcdf(os.path.join(od, f"{var}.nc"))
                ds.close()
            except Exception as e:
                print(f"  skip {m}/{var}: {str(e)[:60]}")


def mean_over(member_files, var, years, clip):
    """
    Average a list of single-variable netCDF files into one annual mean field.

    Each input is collapsed to annual values and reindexed onto the common
    window years before averaging. LAI inputs are already July-only upstream.
    Returns (DataArray, n_used) or (None, 0).
    """
    annual, units = [], ""
    for f in member_files:
        if not os.path.exists(f):
            continue
        da = xr.open_dataset(f)[var]
        units = da.attrs.get("units", units)
        annual.append(da.groupby("time.year").mean("time").reindex(year=list(years)))
    if not annual:
        return None, 0

    ens = xr.concat(annual, dim="member").mean("member", skipna=True)
    if clip:
        ens = ens.clip(min=0)

    yrs = ens["year"].values
    ens = ens.rename({"year": "time"}).assign_coords(
        time=pd.to_datetime([f"{y}-07-01" for y in yrs])
    )
    ens.attrs["units"] = units
    ens.name = var
    ens["lat"].attrs.update({"units": "degrees_north", "standard_name": "latitude"})
    ens["lon"].attrs.update({"units": "degrees_east", "standard_name": "longitude"})
    return ens, len(annual)


def write_ensemble(dst, name, files_for_var, years, clip):
    """Build and write one ensemble-mean directory (name/{var}.nc)."""
    od = os.path.join(dst, name)
    os.makedirs(od, exist_ok=True)
    for var in VARS:
        files = files_for_var(var)
        ens, n = mean_over(files, var, years, clip)
        if ens is not None:
            xr.Dataset({var: ens}).to_netcdf(os.path.join(od, f"{var}.nc"))
            print(f"  {name} {var}: {n} members")


def main():
    ap = argparse.ArgumentParser(description="Build per-window model ensembles.")
    ap.add_argument("window", choices=WINDOW_DEFAULTS.keys(),
                    help="Named evaluation window; sets default --src and years.")
    ap.add_argument("--src", default=None,
                    help="Directory of full-length CMIP6/TRENDY model fields.")
    ap.add_argument("--pecan-members-dir", default=None,
                    help="Directory of per-member PEcAn fields (PEcAn-NNN/). "
                         "If omitted, the PEcAn ensemble mean is skipped.")
    ap.add_argument("--manifest", default="trendy_ensemble_manifest.json",
                    help="JSON listing the screened TRENDY members per variable.")
    ap.add_argument("--dst", default=None,
                    help="Output directory (default: ilamb_models_<window>).")
    args = ap.parse_args()

    defaults = WINDOW_DEFAULTS[args.window]
    src = args.src or defaults["src"]
    year0, year1 = defaults["years"]
    years = list(range(year0, year1 + 1))
    dst = args.dst or f"ilamb_models_{args.window}"

    manifest = json.load(open(args.manifest))
    models = list_individual_models(src)
    print(f"=== slicing {len(models)} models to {year0}-{year1} ===")
    slice_models(src, dst, models, year0, year1)

    cmip6 = [m for m in models if not m.startswith("TRENDY-") and m != "PEcAn"]

    print(f"=== building ensemble means ({year0}-{year1}) ===")
    write_ensemble(dst, "CMIP6-ensemble",
                   lambda v: [os.path.join(dst, m, f"{v}.nc") for m in cmip6],
                   years, clip=False)
    write_ensemble(dst, "TRENDY-ensemble",
                   lambda v: [os.path.join(dst, f"TRENDY-{m}", f"{v}.nc")
                              for m in manifest[v]],
                   years, clip=True)
    if args.pecan_members_dir:
        write_ensemble(
            dst, "PEcAn",
            lambda v: sorted(glob.glob(
                os.path.join(args.pecan_members_dir, "PEcAn-*", f"{v}.nc"))),
            years, clip=False)
    else:
        print("  (PEcAn ensemble mean skipped; pass --pecan-members-dir to build it)")

    print("Done.")


if __name__ == "__main__":
    main()
