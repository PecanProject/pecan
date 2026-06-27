"""
PEcAn vs CMIP6 vs TRENDY — ensemble skill spread figures.
GSoC 2026 PEcAn/ILAMB. Reads the two locked ILAMB scores.csv files
(2012-2014 full-CMIP6 window, 2015-2023 ssp window) and plots the
100-member PEcAn skill cloud against the CMIP6 and TRENDY clouds.
"""
import csv, numpy as np
import matplotlib
matplotlib.use("Agg")
import matplotlib.pyplot as plt

WINDOWS = {
    "2012-2014": "ilamb_results_2012_2014_members/scores.csv",
    "2015-2023": "ilamb_results_2015_2023_members_v2/scores.csv",
}
VARS = ["Biomass", "Leaf Area Index", "Soil Carbon"]
COL = {"PEcAn": "#1f77b4", "CMIP6": "#d62728", "TRENDY": "#2ca02c"}

def load(path):
    rows = list(csv.reader(open(path))); hdr = rows[0]
    idx = {h: i for i, h in enumerate(hdr)}
    pec = [h for h in hdr if h.startswith("PEcAn-")]
    cm  = [h for h in hdr[1:] if not h.startswith("PEcAn") and not h.startswith("TRENDY") and h != "CMIP6-ensemble"]
    tr  = [h for h in hdr if h.startswith("TRENDY-") and h != "TRENDY-ensemble"]
    out = {}
    for r in rows[1:]:
        def grab(names):
            return np.array([float(r[idx[m]]) for m in names if r[idx[m]] not in ("", "--")])
        def one(name):
            v = r[idx[name]]; return float(v) if v not in ("", "--") else np.nan
        out[r[0]] = dict(P=grab(pec), C=grab(cm), T=grab(tr),
                         Pm=one("PEcAn"), Cm=one("CMIP6-ensemble"), Tm=one("TRENDY-ensemble"))
    return out

data = {w: load(p) for w, p in WINDOWS.items()}

fig, axes = plt.subplots(1, len(VARS), figsize=(15, 5.5))
rng = np.random.default_rng(0)
for ax, var in zip(axes, VARS):
    xpos = {"2012-2014": 0, "2015-2023": 1}
    for w in WINDOWS:
        d = data[w][var]; base = xpos[w]
        for j, (grp, key) in enumerate([("PEcAn","P"),("CMIP6","C"),("TRENDY","T")]):
            vals = d[key]
            if len(vals) == 0: continue
            x = base*3 + j + rng.normal(0, 0.06, len(vals))
            ax.scatter(x, vals, s=14, alpha=0.55, color=COL[grp], edgecolors="none",
                       zorder=2, label=grp if w=="2012-2014" else None)
            ax.hlines(np.median(vals), base*3+j-0.25, base*3+j+0.25, color=COL[grp], lw=2.5, zorder=3)
        if np.isfinite(d["Pm"]):
            ax.scatter([base*3+0], [d["Pm"]], marker="*", s=240, color="gold",
                       edgecolors="k", linewidths=0.8, zorder=5)
    ax.set_title(var, fontsize=13, fontweight="bold")
    ax.set_xticks([1, 4]); ax.set_xticklabels(["2012-2014\n(25 CMIP6)", "2015-2023\n(14 CMIP6)"])
    ax.set_ylabel("ILAMB score (higher = better)")
    ax.grid(axis="y", alpha=0.25)
    ax.axvline(2.5, color="0.85", lw=1)
axes[0].legend(loc="lower left", framealpha=0.9, fontsize=10)
fig.suptitle("Ensemble skill spread: PEcAn (100 SDA members) vs CMIP6 vs TRENDY\n"
             "gold star = PEcAn ensemble-mean; bars = group medians", fontsize=13, y=1.02)
fig.tight_layout()
fig.savefig("fig_spread_clouds.png", dpi=160, bbox_inches="tight")
print("wrote fig_spread_clouds.png")


print("\n=== numbers behind the figures ===")
for w in WINDOWS:
    print(f"\n[{w}]")
    for v in VARS:
        d=data[w][v]
        def fmt(k):
            a=d[k]; return f"n={len(a)} med={np.median(a):.3f} sd={a.std():.4f}" if len(a) else "n=0"
        print(f"  {v:16s} PEcAn(mean={d['Pm']:.3f}) {fmt('P')} | CMIP6 {fmt('C')} | TRENDY {fmt('T')}")
