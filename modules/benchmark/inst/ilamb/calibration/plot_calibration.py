"""
Example: make a calibration figure from the diagnostics.

Produces a two-panel figure comparing the calibration of several ensembles:

  Panel (a)  the bias-corrected spread-to-error ratio per variable (a value
             near one means the spread is the right size)
  Panel (b)  the fraction of observations inside the ensemble 90 percent band
             per variable (near 0.90 for a calibrated ensemble)

Both panels are directly comparable across ensembles regardless of member
count, which is why the spread ratio and coverage are used here rather than
raw rank histograms (see the note in README.md).

This script additionally requires matplotlib and xarray, which the core
diagnostics in ensemble_calibration.py do not. Install with
`pip install matplotlib xarray`.

NOTE: as in example_calibration.py, the directory names, benchmark paths, and
the ILAMB_ROOT environment variable below reflect one particular benchmarking
layout and are illustrative. Adapt them to your own data before running.
"""

import os
import glob
import json
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import xarray as xr
import ensemble_calibration as ec

# variable -> (benchmark glob, benchmark variable, unit conversion to model units)
BENCH = {
    "cVeg":  ("DATA/biomass/XuSaatchi2021/*.nc", "biomass", 0.1),
    "cSoil": ("DATA/cSoil/HWSD2/*.nc",           "cSoil",   1.0),
    "lai":   ("DATA/lai/GIMMS_LAI4g/*.nc",       "lai",     1.0),
}
VAR_LABEL = {"cVeg": "Biomass", "cSoil": "Soil Carbon", "lai": "LAI"}

# Adapt these to your layout. Here we plot a single evaluation window.
WINDOW = dict(root="ilamb_models_2015_2023", pecan="ilamb_models_members")

COLORS = {"PEcAn": "#c0392b", "CMIP6": "#2980b9", "TRENDY": "#27ae60"}


def load_field(path, var):
    """Time-mean 2D field for a variable from one netCDF, or None."""
    if not os.path.exists(path):
        return None
    d = xr.open_dataset(path, decode_times=False)
    if var not in d:
        return None
    a = d[var]
    return a.mean("time") if "time" in a.dims else a


def load_bench(var, tgt, ilamb_root):
    """Load the benchmark for `var` and subset it to the target member grid."""
    pat, bvar, scale = BENCH[var]
    a = xr.open_dataset(glob.glob(os.path.join(ilamb_root, pat))[0],
                        decode_times=False)[bvar]
    ren = {}
    for c in a.coords:
        cl = str(c).lower()
        if cl in ("latitude", "y"): ren[c] = "lat"
        if cl in ("longitude", "x"): ren[c] = "lon"
    a = a.rename(ren)
    if "time" in a.dims:
        a = a.mean("time")
    return a.sel(lat=tgt.lat, lon=tgt.lon, method="nearest").values * scale


def stack_members(files, var):
    fields = []
    for f in files:
        a = load_field(f, var)
        if a is not None:
            fields.append(a.values)
    return np.stack(fields, axis=0) if fields else None


def ensembles_for(var, manifest):
    root, pecan = WINDOW["root"], WINDOW["pecan"]
    pecan_files = sorted(glob.glob(f"{pecan}/PEcAn-*/{var}.nc"))
    ens_dirs = {"CMIP6-ensemble", "TRENDY-ensemble", "PEcAn"}
    models = [d for d in sorted(os.listdir(root))
              if os.path.isdir(os.path.join(root, d)) and d not in ens_dirs]
    cmip6_files = [os.path.join(root, m, f"{var}.nc")
                   for m in models if not m.startswith("TRENDY-")]
    trendy_files = [os.path.join(root, f"TRENDY-{m}", f"{var}.nc")
                    for m in manifest[var]]
    return {"PEcAn": pecan_files, "CMIP6": cmip6_files, "TRENDY": trendy_files}


def bias_removed_ratio(members, obs):
    """Spread-to-error ratio with the mean bias removed from the error."""
    n = members.shape[0]
    mask = np.isfinite(obs) & np.all(np.isfinite(members), axis=0)
    m = members[:, mask]
    o = obs[mask]
    mean = m.mean(axis=0)
    std = m.std(axis=0, ddof=1)
    spread = np.sqrt((n + 1) / n) * std.mean()
    err = mean - o
    crmse = np.sqrt(np.mean((err - err.mean()) ** 2))
    return spread / crmse if crmse > 0 else np.nan


def main():
    ilamb_root = os.environ["ILAMB_ROOT"]
    manifest = json.load(open("trendy_ensemble_manifest.json"))

    variables = ["cVeg", "cSoil", "lai"]
    names = ["PEcAn", "CMIP6", "TRENDY"]
    ratio = {nm: [] for nm in names}
    cov = {nm: [] for nm in names}

    for var in variables:
        ens = ensembles_for(var, manifest)
        tgt = load_field(ens["PEcAn"][0], var)
        obs = load_bench(var, tgt, ilamb_root)
        for nm in names:
            M = stack_members(ens[nm], var)
            if M is not None and M.shape[0] >= 3:
                ratio[nm].append(bias_removed_ratio(M, obs))
                cov[nm].append(ec.coverage(M, obs, interval=0.9)["coverage"])
            else:
                ratio[nm].append(np.nan)
                cov[nm].append(np.nan)

    fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(12, 4.6))
    x = np.arange(len(variables))
    w = 0.25

    for i, nm in enumerate(names):
        ax1.bar(x + (i - 1) * w, ratio[nm], w, label=nm, color=COLORS[nm])
    ax1.axhline(1.0, color="k", ls="--", lw=1)
    ax1.text(len(variables) - 0.65, 1.03, "well-calibrated", fontsize=7, ha="right")
    ax1.set_xticks(x)
    ax1.set_xticklabels([VAR_LABEL[v] for v in variables])
    ax1.set_ylabel("ensemble spread / error  (bias-removed)")
    ax1.set_title("(a) Is the spread the right size?")
    ax1.legend(fontsize=8, loc="upper left")
    ax1.set_ylim(0, 1.8)

    for i, nm in enumerate(names):
        ax2.bar(x + (i - 1) * w, cov[nm], w, label=nm, color=COLORS[nm])
    ax2.axhline(0.9, color="k", ls="--", lw=1)
    ax2.text(len(variables) - 0.65, 0.92, "expected (0.90)", fontsize=7, ha="right")
    ax2.set_xticks(x)
    ax2.set_xticklabels([VAR_LABEL[v] for v in variables])
    ax2.set_ylabel("fraction of observations inside 90% band")
    ax2.set_title("(b) Does the ensemble contain the observation?")
    ax2.legend(fontsize=8, loc="upper left")
    ax2.set_ylim(0, 1.0)

    plt.tight_layout()
    plt.savefig("calibration_figure.png", dpi=150, bbox_inches="tight")
    print("wrote calibration_figure.png")


if __name__ == "__main__":
    main()
