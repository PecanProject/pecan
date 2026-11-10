"""
Figures for the ESACCI biomass benchmark extension.

Produces:
  fig_benchmark_comparison.png   ESACCI-carbon vs XuSaatchi per grid cell (carbon
                                 conversion validation), coloured by biomass level
  fig_three_benchmarks.png       calibration (ratio + coverage) against XuSaatchi,
                                 ESACCI 2020, ESACCI 2024 (the cross-validation)
  fig_obs_error.png              coverage and ratio raw vs with observation error
                                 (the headline result)

Reuses the sampling and obs-error functions from score_esacci.py so the figure
numbers match the tables.
"""
import os
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import xarray as xr

import ensemble_calibration as ec
from score_esacci import (
    load_sites, sample_ensemble, sample_nc,
    coverage_with_obs_error, spread_skill_with_obs_error,
)

OUT = "figures"
os.makedirs(OUT, exist_ok=True)


def fig_benchmark_comparison():
    """ESACCI-carbon vs XuSaatchi per grid cell, with 1:1 line."""
    R = os.environ["ILAMB_ROOT"]
    xu = xr.open_dataset(f"{R}/DATA/biomass/XuSaatchi2021/XuSaatchi.nc", decode_times=False)
    esa = xr.open_dataset("ESACCI_biomass_0.5deg.nc")
    e = esa["biomass"].isel(time=5).values          # 2020, Mg C/ha
    x = xu["biomass"].mean("time").values           # Mg C/ha
    m = np.isfinite(e) & np.isfinite(x) & (x > 1) & (e > 1)

    fig, ax = plt.subplots(figsize=(7, 7))
    ax.scatter(x[m], e[m], s=3, alpha=0.15, color="#2c7fb8")
    lim = 250
    ax.plot([0, lim], [0, lim], "k--", lw=1, label="1:1")
    ax.set_xlim(0, lim); ax.set_ylim(0, lim)
    ax.set_xlabel("XuSaatchi carbon density (Mg C/ha)")
    ax.set_ylabel("ESACCI carbon density (Mg C/ha)")
    ax.set_title("ESACCI vs XuSaatchi biomass benchmark\n"
                 "(agreement improves toward dense forest; 0.47 carbon fraction)")
    ax.legend()
    plt.tight_layout()
    plt.savefig(f"{OUT}/fig_benchmark_comparison.png", dpi=150, bbox_inches="tight")
    plt.close()
    print(f"wrote {OUT}/fig_benchmark_comparison.png")


def fig_three_benchmarks(members, benches):
    """Bar chart: ratio and coverage against three benchmarks."""
    names = list(benches.keys())
    ratios = [ec.spread_skill(members, benches[n])["ratio"] for n in names]
    covs = [ec.coverage(members, benches[n], 0.9)["coverage"] for n in names]
    x = np.arange(len(names))
    fig, (a1, a2) = plt.subplots(1, 2, figsize=(11, 5))
    a1.bar(x, ratios, color=["#7fbf7b", "#2c7fb8", "#1a5276"])
    a1.axhline(1.0, color="k", ls="--", lw=1); a1.text(len(names)-1, 1.02, "calibrated (1.0)", ha="right", fontsize=8)
    a1.set_xticks(x); a1.set_xticklabels(names, rotation=20, ha="right", fontsize=9)
    a1.set_ylabel("spread / error"); a1.set_ylim(0, 1.1); a1.set_title("Spread-to-error ratio")
    a2.bar(x, covs, color=["#7fbf7b", "#2c7fb8", "#1a5276"])
    a2.axhline(0.9, color="k", ls="--", lw=1); a2.text(len(names)-1, 0.92, "expected (0.90)", ha="right", fontsize=8)
    a2.set_xticks(x); a2.set_xticklabels(names, rotation=20, ha="right", fontsize=9)
    a2.set_ylabel("90% coverage"); a2.set_ylim(0, 1.0); a2.set_title("90% coverage")
    fig.suptitle("Ensemble overconfident against every biomass benchmark (cross-validation)", fontsize=12)
    plt.tight_layout()
    plt.savefig(f"{OUT}/fig_three_benchmarks.png", dpi=150, bbox_inches="tight")
    plt.close()
    print(f"wrote {OUT}/fig_three_benchmarks.png")


def fig_obs_error(members, obs, obs_sd, disagree):
    """Bar chart: coverage and ratio raw vs with obs error (the headline)."""
    labels = ["raw", "+ ESACCI\nper-pixel SD", "+ XuSaatchi-ESACCI\ndisagreement"]
    covs = [ec.coverage(members, obs, 0.9)["coverage"],
            coverage_with_obs_error(members, obs, obs_sd),
            coverage_with_obs_error(members, obs, disagree)]
    ratios = [ec.spread_skill(members, obs)["ratio"],
              spread_skill_with_obs_error(members, obs, obs_sd),
              spread_skill_with_obs_error(members, obs, disagree)]
    x = np.arange(len(labels))
    fig, (a1, a2) = plt.subplots(1, 2, figsize=(11, 5))
    a1.bar(x, covs, color="#2c7fb8")
    a1.axhline(0.9, color="k", ls="--", lw=1); a1.text(len(labels)-1, 0.92, "expected (0.90)", ha="right", fontsize=8)
    a1.set_xticks(x); a1.set_xticklabels(labels, fontsize=8)
    a1.set_ylabel("90% coverage"); a1.set_ylim(0, 1.0); a1.set_title("90% coverage")
    a2.bar(x, ratios, color="#c0392b")
    a2.axhline(1.0, color="k", ls="--", lw=1); a2.text(len(labels)-1, 1.02, "calibrated (1.0)", ha="right", fontsize=8)
    a2.set_xticks(x); a2.set_xticklabels(labels, fontsize=8)
    a2.set_ylabel("spread / error"); a2.set_ylim(0, 1.1); a2.set_title("Spread-to-error ratio")
    fig.suptitle("Overconfidence survives observation error (coverage stays far below 0.9)", fontsize=12)
    plt.tight_layout()
    plt.savefig(f"{OUT}/fig_obs_error.png", dpi=150, bbox_inches="tight")
    plt.close()
    print(f"wrote {OUT}/fig_obs_error.png")


def main():
    lon, lat = load_sites()
    print(f"sampling ensemble at {len(lon)} sites...")
    members = sample_ensemble(lon, lat)
    R = os.environ["ILAMB_ROOT"]
    xu = sample_nc(f"{R}/DATA/biomass/XuSaatchi2021/XuSaatchi.nc", "biomass", lon, lat, None, 0.1)
    esa20 = sample_nc("ESACCI_biomass_0.5deg.nc", "biomass", lon, lat, 5, 0.1)
    esa24 = sample_nc("ESACCI_biomass_0.5deg.nc", "biomass", lon, lat, 9, 0.1)
    obs_sd = sample_nc("ESACCI_biomass_0.5deg.nc", "biomass_sd", lon, lat, 5, 0.1)
    disagree = np.abs(xu - esa20)

    fig_benchmark_comparison()
    fig_three_benchmarks(members, {"XuSaatchi\n(~2010)": xu, "ESACCI\n2020": esa20, "ESACCI\n2024": esa24})
    fig_obs_error(members, esa20, obs_sd, disagree)


if __name__ == "__main__":
    main()
