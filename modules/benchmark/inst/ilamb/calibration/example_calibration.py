"""
Example: run the ensemble calibration diagnostics on real ensembles.

This shows how to apply ensemble_calibration to the PEcAn, CMIP6, and TRENDY
ensembles for two evaluation windows. For each variable and window it loads the
ensemble members (PEcAn: the 100 data-assimilation members; CMIP6/TRENDY: the
individual models), aligns the observational benchmark to the member grid, and
reports the spread-skill ratio and central coverage (both directly comparable
across ensembles) plus the rank-histogram shape.

Members and benchmark are compared as time-means: the benchmarks are largely
static snapshots (biomass ~2010, soil carbon time-invariant, LAI climatology),
so each member's field is averaged over the window before comparison, matching
how ILAMB scores them.

NOTE: the directory names, benchmark paths, and the ILAMB_ROOT environment
variable below reflect one particular benchmarking layout. They are
illustrative. Adapt the paths, window definitions, and benchmark locations to
your own data before running. The calibration functions themselves in
ensemble_calibration.py are independent of any of this layout.
"""

import os
import glob
import json
import numpy as np
import xarray as xr
import ensemble_calibration as ec

# variable -> (benchmark glob, benchmark variable name)
BENCH = {
    "cVeg":  ("DATA/biomass/XuSaatchi2021/*.nc", "biomass"),
    "cSoil": ("DATA/cSoil/HWSD2/*.nc",           "cSoil"),
    "lai":   ("DATA/lai/GIMMS_LAI4g/*.nc",       "lai"),
}
VAR_LABEL = {"cVeg": "Biomass", "cSoil": "Soil Carbon", "lai": "Leaf Area Index"}

# Benchmark unit conversions to match model units (kg m-2, m2 m-2).
# For example, XuSaatchi biomass is Mg ha-1 while the models are kg m-2;
# 1 kg m-2 = 10 Mg ha-1, so the benchmark is multiplied by 0.1.
BENCH_SCALE = {"cVeg": 0.1, "cSoil": 1.0, "lai": 1.0}

# Window definitions: for each window, the directory holding the individual
# models and the directory holding the PEcAn members. Adapt to your layout.
WINDOWS = {
    "2012_2014": dict(root="ilamb_models_2012_2014",
                      pecan="ilamb_models_members_2012_2014"),
    "2015_2023": dict(root="ilamb_models_2015_2023",
                      pecan="ilamb_models_members"),
}


def load_field(path, var):
    """Time-mean 2D field (lat, lon) for a variable from one netCDF, or None."""
    if not os.path.exists(path):
        return None
    d = xr.open_dataset(path, decode_times=False)
    if var not in d:
        return None
    a = d[var]
    if "time" in a.dims:
        a = a.mean("time")
    return a


def load_bench(var, tgt, ilamb_root):
    """Load the benchmark for `var` and subset it to the target member grid."""
    pat, bvar = BENCH[var]
    f = glob.glob(os.path.join(ilamb_root, pat))[0]
    d = xr.open_dataset(f, decode_times=False)
    a = d[bvar]
    # rename any lat/lon variants to a common naming
    ren = {}
    for c in a.coords:
        cl = str(c).lower()
        if cl in ("latitude", "y"): ren[c] = "lat"
        if cl in ("longitude", "x"): ren[c] = "lon"
    a = a.rename(ren)
    if "time" in a.dims:
        a = a.mean("time")
    # subset global benchmark to the member grid (same 0.5 deg cell centers)
    a = a.sel(lat=tgt.lat, lon=tgt.lon, method="nearest")
    return a.values * BENCH_SCALE[var]


def stack_members(files, var):
    """Stack per-member/model time-mean fields into (n, lat, lon)."""
    fields = []
    for f in files:
        a = load_field(f, var)
        if a is not None:
            fields.append(a.values)
    if not fields:
        return None
    return np.stack(fields, axis=0)


def ensembles_for(window, var, manifest):
    """Return {name: member_files} for PEcAn, CMIP6, TRENDY in this window."""
    cfg = WINDOWS[window]
    root, pecan = cfg["root"], cfg["pecan"]
    # PEcAn: the member directories
    pecan_files = sorted(glob.glob(f"{pecan}/PEcAn-*/{var}.nc"))
    # individual models under the window root, minus ensembles and the PEcAn mean
    ens_dirs = {"CMIP6-ensemble", "TRENDY-ensemble", "PEcAn"}
    models = [d for d in sorted(os.listdir(root))
              if os.path.isdir(os.path.join(root, d)) and d not in ens_dirs]
    cmip6_files = [os.path.join(root, m, f"{var}.nc")
                   for m in models if not m.startswith("TRENDY-")]
    trendy_files = [os.path.join(root, f"TRENDY-{m}", f"{var}.nc")
                    for m in manifest[var]]
    return {"PEcAn": pecan_files, "CMIP6": cmip6_files, "TRENDY": trendy_files}


def hist_shape(counts):
    """Label a rank histogram shape from its normalized bins."""
    f = counts / counts.mean()
    ends = (f[0] + f[-1]) / 2
    mid = f[len(f) // 2]
    if ends > 1.5 and ends > mid: return "U (overconfident)"
    if mid > 1.5 and mid > ends:  return "dome (underconfident)"
    return "~flat (calibrated)"


def main():
    # Loaded here (not at import time) so importing this module never depends
    # on a data file being present.
    ilamb_root = os.environ["ILAMB_ROOT"]
    manifest = json.load(open("trendy_ensemble_manifest.json"))

    for window in WINDOWS:
        print("=" * 70)
        print(f"WINDOW {window}")
        print("=" * 70)
        for var in ["cVeg", "cSoil", "lai"]:
            print(f"\n{VAR_LABEL[var]} ({var})")
            ens = ensembles_for(window, var, manifest)
            # load benchmark on the PEcAn grid (use a PEcAn member as grid target)
            tgt = load_field(ens["PEcAn"][0], var)
            obs = load_bench(var, tgt, ilamb_root)
            print(f"  {'ensemble':8s} {'n':>4s}  {'spread/rmse':>11s}  {'cov90':>6s}  rank-hist")
            for name in ["PEcAn", "CMIP6", "TRENDY"]:
                M = stack_members(ens[name], var)
                if M is None or M.shape[0] < 3:
                    print(f"  {name:8s} {'--':>4s}  (too few members)")
                    continue
                ss = ec.spread_skill(M, obs)
                cov = ec.coverage(M, obs, interval=0.9)
                counts, _ = ec.rank_histogram(M, obs)
                print(f"  {name:8s} {M.shape[0]:4d}  {ss['ratio']:11.3f}  "
                      f"{cov['coverage']:6.3f}  {hist_shape(counts)}")


if __name__ == "__main__":
    main()
