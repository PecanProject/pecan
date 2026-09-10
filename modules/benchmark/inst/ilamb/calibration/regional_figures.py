"""
Figures for the regional calibration diagnostics.

Produces choropleth maps of Level 1 ecoregions shaded by calibration, bar charts
of calibration by land cover class and by ecoregion, and a map of the sites
coloured by land cover class. Reuses the sampling and stratification from
regional_diagnostics.py and the ecoregion assignment from ecoregion_join.py, so
the numbers in the figures match the tables exactly.

Requires matplotlib and geopandas in addition to the core diagnostics. Figures
are written to the OUT directory. Paths follow the other regional modules and are
illustrative; adapt them to your own layout.
"""

import os
import numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt
import geopandas as gpd

import ensemble_calibration as ec
from regional_diagnostics import (
    load_sites, sample_ensemble, sample_benchmark,
    stratified_calibration, VARIABLES, VAR_LABEL, LANDCOVER_LABELS,
)
from ecoregion_join import assign_ecoregions, isnull, ECO_DIR

ECO_L1 = os.path.join(ECO_DIR, "NA_CEC_Eco_Level1.shp")
OUT = "figures"
os.makedirs(OUT, exist_ok=True)
RATIO_CMAP = "RdYlGn"   # red = low ratio (overconfident), green = near 1
COV_CMAP = "RdYlGn"     # red = low coverage, green = near expected


def compute_region_metrics(names, ens_cache, var, min_sites=30):
    """Return {region_name: dict(ratio, cov90, n)} for one variable."""
    members, obs = ens_cache[var]
    out = {}
    for rn in sorted(set(names[~isnull(names)])):
        sel = np.array([n == rn for n in names])
        if sel.sum() < min_sites:
            continue
        m = members[:, sel]
        o = obs[sel]
        out[rn] = dict(ratio=ec.spread_skill(m, o)["ratio"],
                       cov90=ec.coverage(m, o, interval=0.9)["coverage"],
                       n=int(sel.sum()))
    return out


def draw_choropleth(dissolved, region_metrics, metric, cmap, vmin, vmax,
                    title, fname, cbar_label):
    """Shade dissolved L1 polygons by a metric and save."""
    vals = dissolved.index.map(lambda rn: region_metrics.get(rn, {}).get(metric, np.nan))
    dissolved = dissolved.copy()
    dissolved["_metric"] = list(vals)
    fig, ax = plt.subplots(1, 1, figsize=(11, 9))
    dissolved.plot(ax=ax, color="#eeeeee", edgecolor="#999999", linewidth=0.3)
    have = dissolved[dissolved["_metric"].notna()]
    have.plot(ax=ax, column="_metric", cmap=cmap, edgecolor="#666666",
              linewidth=0.3, vmin=vmin, vmax=vmax, legend=True,
              legend_kwds={"label": cbar_label, "shrink": 0.5})
    ax.set_xlim(-170, -52)
    ax.set_ylim(14, 84)
    ax.set_title(title, fontsize=13)
    ax.set_xlabel("longitude")
    ax.set_ylabel("latitude")
    plt.tight_layout()
    plt.savefig(f"{OUT}/{fname}", dpi=150, bbox_inches="tight")
    plt.close()
    print(f"wrote {OUT}/{fname}")


def bars_by_group(rows_by_var, title, fname, ref_ratio=1.0):
    """Grouped bar chart with a ratio panel and a coverage panel."""
    variables = list(rows_by_var.keys())
    labels = [r["label"] for r in rows_by_var[variables[0]]]
    x = np.arange(len(labels))
    w = 0.25
    colors = {"biomass": "#c0392b", "cSoil": "#8e44ad", "lai": "#27ae60"}
    fig, (ax1, ax2) = plt.subplots(2, 1, figsize=(max(10, len(labels) * 0.7), 9))
    for i, var in enumerate(variables):
        rmap = {r["label"]: r for r in rows_by_var[var]}
        ratios = [rmap.get(l, {}).get("ratio", np.nan) for l in labels]
        covs = [rmap.get(l, {}).get("cov90", np.nan) for l in labels]
        ax1.bar(x + (i - 1) * w, ratios, w, label=VAR_LABEL[var], color=colors[var])
        ax2.bar(x + (i - 1) * w, covs, w, label=VAR_LABEL[var], color=colors[var])
    ax1.axhline(ref_ratio, color="k", ls="--", lw=1)
    ax1.text(len(labels) - 0.5, ref_ratio + 0.02, "well-calibrated (1.0)",
             fontsize=8, ha="right")
    ax1.set_ylabel("spread / error")
    ax1.set_title(f"{title}: spread-to-error ratio (lower = more overconfident)")
    ax1.set_xticks(x)
    ax1.set_xticklabels([])
    ax1.legend(fontsize=9)
    ax1.set_ylim(0, max(1.2, ref_ratio + 0.2))
    ax2.axhline(0.9, color="k", ls="--", lw=1)
    ax2.text(len(labels) - 0.5, 0.92, "expected (0.90)", fontsize=8, ha="right")
    ax2.set_ylabel("fraction of obs in 90% band")
    ax2.set_title("90 percent coverage")
    ax2.set_xticks(x)
    ax2.set_xticklabels(labels, rotation=45, ha="right", fontsize=8)
    ax2.legend(fontsize=9)
    ax2.set_ylim(0, 1.0)
    plt.tight_layout()
    plt.savefig(f"{OUT}/{fname}", dpi=150, bbox_inches="tight")
    plt.close()
    print(f"wrote {OUT}/{fname}")


def map_sites_by_landcover(lon, lat, lc):
    """Scatter the sites coloured by land cover class over the ecoregion outline."""
    base = gpd.read_file(ECO_L1).to_crs("EPSG:4326").dissolve()
    fig, ax = plt.subplots(1, 1, figsize=(11, 9))
    base.plot(ax=ax, color="#f5f5f5", edgecolor="#bbbbbb", linewidth=0.4)
    cmap = plt.get_cmap("tab10")
    for c in sorted(set(lc)):
        sel = lc == c
        ax.scatter(lon[sel], lat[sel], s=4, color=cmap((c - 1) % 10),
                   label=f"{c} {LANDCOVER_LABELS.get(c, '')}", alpha=0.7)
    ax.set_xlim(-170, -52)
    ax.set_ylim(14, 84)
    ax.set_title("SDA sites by land cover class", fontsize=13)
    ax.set_xlabel("longitude")
    ax.set_ylabel("latitude")
    ax.legend(fontsize=7, loc="lower left", markerscale=2)
    plt.tight_layout()
    plt.savefig(f"{OUT}/fig_sites_by_landcover.png", dpi=150, bbox_inches="tight")
    plt.close()
    print(f"wrote {OUT}/fig_sites_by_landcover.png")


def main():
    lon, lat, lc = load_sites()
    ens_cache = {}
    for var, (map_var, bglob, bvar, scale) in VARIABLES.items():
        ens_cache[var] = (sample_ensemble(map_var, lon, lat),
                          sample_benchmark(bglob, bvar, scale, lon, lat))

    map_sites_by_landcover(lon, lat, lc)

    lc_rows = {var: stratified_calibration(*ens_cache[var], lc, LANDCOVER_LABELS)
               for var in VARIABLES}
    bars_by_group(lc_rows, "Calibration by land cover class", "fig_landcover_bars.png")

    eco = assign_ecoregions(lon, lat)
    l1 = eco["L1"]
    uniq = sorted(set(l1[~isnull(l1)]))
    name_to_id = {n: i for i, n in enumerate(uniq)}
    labels = {i: n for n, i in name_to_id.items()}
    groups = np.array([name_to_id.get(n, -1) for n in l1])
    eco_rows = {}
    for var in VARIABLES:
        rows = stratified_calibration(*ens_cache[var], groups, labels)
        eco_rows[var] = [r for r in sorted(rows, key=lambda r: -r["n"]) if r["n"] >= 30]
    bars_by_group(eco_rows, "Calibration by L1 ecoregion", "fig_ecoregion_bars.png")

    dissolved = gpd.read_file(ECO_L1).to_crs("EPSG:4326").dissolve(by="NA_L1NAME")
    for var in VARIABLES:
        rm = compute_region_metrics(l1, ens_cache, var)
        draw_choropleth(dissolved, rm, "ratio", RATIO_CMAP, 0.0, 1.0,
                        f"{VAR_LABEL[var]}: ensemble spread / error by ecoregion\n"
                        f"(red = overconfident, green = well sized)",
                        f"fig_ecoregion_map_{var}_ratio.png", "spread / error")
        draw_choropleth(dissolved, rm, "cov90", COV_CMAP, 0.0, 0.9,
                        f"{VAR_LABEL[var]}: fraction of obs in 90% band by ecoregion\n"
                        f"(red = obs outside, green = near 0.90)",
                        f"fig_ecoregion_map_{var}_cov.png", "90% coverage")


if __name__ == "__main__":
    main()
