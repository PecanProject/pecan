"""
Regional calibration diagnostics.

Breaks the ensemble calibration assessment down by region rather than reporting a
single domain-wide number. The core engine, `stratified_calibration`, is grouping
agnostic: given the ensemble and the observation sampled at a set of point
locations plus a group label for each location, it computes the calibration
diagnostics per group. Land cover is the first grouping applied here; the
ecoregion grouping in ecoregion_join.py plugs into the same engine.

This reuses the diagnostics in ensemble_calibration.py (spread_skill, coverage),
so every per-region number is computed the same way as the domain-wide ones,
just on subsets of the sites.

    Ensemble  the per-member downscaled maps sampled at the site coordinates
    Obs       the observational benchmark sampled at the same coordinates
    Group     the land cover class of each site (from the site shapefile)

NOTE ON PATHS: the directory locations below (the SDA output directory, the
downscaled-map layout, the benchmark locations, and the ILAMB_ROOT environment
variable) reflect one particular benchmarking setup and are illustrative. Adapt
them to your own data before running. The stratification engine itself is
independent of any of this layout.

NOTE ON LABELS: LANDCOVER_LABELS are the MODIS MCD12Q1 PFT classes from the
North American carbon reanalysis (Zhang et al.), with the class integers 1-8
following the standard MODIS PFT coding, confirmed against site geography. The
labels are display strings only and do not affect the computed numbers.
"""

import os
import glob
import numpy as np
import ensemble_calibration as ec

# fiona, rasterio, and xarray are imported lazily inside the functions that read
# data files, so the stratification engine (and its tests) need only NumPy and
# the calibration module.

# ----------------------------------------------------------------------------
# Configuration (adapt to your layout)
# ----------------------------------------------------------------------------

# Root of the SDA output, its downscaled maps, and the site shapefile.
SDA_DIR = "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site"
SHAPEFILE = f"{SDA_DIR}/shapefile/pts.shp"
MAPS_DIR = f"{SDA_DIR}/downscale_maps_analysis_lc_ts_noGEDI_rf"
YEAR = 2015

# variable -> (downscaled-map variable name, benchmark glob relative to ILAMB_ROOT,
#              benchmark variable name, unit conversion to model units kg m-2 / m2 m-2)
VARIABLES = {
    "biomass": ("AbvGrndWood", "DATA/biomass/XuSaatchi2021/*.nc", "biomass", 0.1),
    "cSoil":   ("TotSoilCarb", "DATA/cSoil/HWSD2/*.nc",           "cSoil",   1.0),
    "lai":     ("LAI",         "DATA/lai/GIMMS_LAI4g/*.nc",       "lai",     1.0),
}
VAR_LABEL = {"biomass": "Biomass", "cSoil": "Soil Carbon", "lai": "Leaf Area Index"}

# Land cover class labels: MODIS MCD12Q1 PFT classification (Zhang et al.,
# North American carbon reanalysis). Class integers 1-8 follow the standard
# MODIS PFT coding, confirmed here against site geography (class 2 southernmost,
# class 3 northernmost, consistent with evergreen broadleaf and larch).
LANDCOVER_LABELS = {
    1: "Evergreen Needleleaf Trees",
    2: "Evergreen Broadleaf Trees",
    3: "Deciduous Needleleaf Trees",
    4: "Deciduous Broadleaf Trees",
    5: "Shrubs",
    6: "Grass",
    7: "Cereal Croplands",
    8: "Broad Croplands",
}


def _ilamb_root():
    """ILAMB_ROOT from the environment, read lazily so importing never fails."""
    return os.environ.get("ILAMB_ROOT", "")


# ----------------------------------------------------------------------------
# Site loading
# ----------------------------------------------------------------------------

def load_sites():
    """Return (lon, lat, landcover) arrays for all sites, in shapefile order."""
    import fiona
    lon, lat, lc = [], [], []
    with fiona.open(SHAPEFILE) as src:
        for r in src:
            x, y = r["geometry"]["coordinates"]
            lon.append(x)
            lat.append(y)
            lc.append(int(r["properties"]["landcover"]))
    return np.array(lon), np.array(lat), np.array(lc)


# ----------------------------------------------------------------------------
# Sampling ensemble and benchmark at the sites
# ----------------------------------------------------------------------------

def sample_raster(path, lon, lat):
    """Sample a single-band raster at the given lon/lat points."""
    import rasterio
    with rasterio.open(path) as r:
        vals = np.array([v[0] for v in r.sample(list(zip(lon, lat)))], dtype=float)
        if r.nodata is not None:
            vals[vals == r.nodata] = np.nan
    return vals


def sample_ensemble(map_var, lon, lat):
    """
    Sample every per-member downscaled map for a variable at the sites.
    Returns an array of shape (n_members, n_sites).
    """
    files = sorted(glob.glob(
        f"{MAPS_DIR}/{map_var}_{YEAR}/ensemble_*_{YEAR}_{map_var}.tiff"))
    if not files:
        raise FileNotFoundError(f"no ensemble maps for {map_var} in {MAPS_DIR}")
    members = [sample_raster(f, lon, lat) for f in files]
    return np.vstack(members)


def sample_benchmark(bench_glob, bench_var, scale, lon, lat):
    """Sample the benchmark netCDF at the sites, with unit conversion."""
    import xarray as xr
    f = glob.glob(os.path.join(_ilamb_root(), bench_glob))[0]
    a = xr.open_dataset(f, decode_times=False)[bench_var]
    ren = {}
    for c in a.coords:
        cl = str(c).lower()
        if cl in ("latitude", "y"): ren[c] = "lat"
        if cl in ("longitude", "x"): ren[c] = "lon"
    a = a.rename(ren)
    if "time" in a.dims:
        a = a.mean("time")
    xa = xr.DataArray(lon, dims="site")
    ya = xr.DataArray(lat, dims="site")
    return a.sel(lat=ya, lon=xa, method="nearest").values * scale


# ----------------------------------------------------------------------------
# The grouping-agnostic stratification engine
# ----------------------------------------------------------------------------

def stratified_calibration(members, obs, groups, labels):
    """
    Compute calibration diagnostics per group.

    members  (n_members, n_sites) ensemble sampled at the sites
    obs      (n_sites,) observation sampled at the sites
    groups   (n_sites,) integer group id for each site
    labels   dict mapping group id -> display name

    Returns a list of per-group dicts with the group id, label, site count, the
    spread-skill ratio, and the central-90 coverage. Groups with fewer than ten
    valid sites are returned with NaN metrics. Uses ensemble_calibration so the
    numbers match the domain-wide diagnostics.
    """
    rows = []
    for g in sorted(labels):
        sel = groups == g
        n_sites = int(sel.sum())
        if n_sites < 10:
            rows.append(dict(group=g, label=labels[g], n=n_sites,
                             ratio=np.nan, cov90=np.nan))
            continue
        m = members[:, sel]
        o = obs[sel]
        ss = ec.spread_skill(m, o)
        cov = ec.coverage(m, o, interval=0.9)
        rows.append(dict(group=g, label=labels[g], n=n_sites,
                         ratio=ss["ratio"], cov90=cov["coverage"]))
    return rows


# ----------------------------------------------------------------------------
# Driver: land cover breakdown for each variable
# ----------------------------------------------------------------------------

def main():
    lon, lat, lc = load_sites()
    print(f"loaded {len(lon)} sites across {len(set(lc))} land cover classes\n")

    for var, (map_var, bglob, bvar, scale) in VARIABLES.items():
        print("=" * 72)
        print(f"{VAR_LABEL[var]}  ({var})")
        print("=" * 72)
        members = sample_ensemble(map_var, lon, lat)
        obs = sample_benchmark(bglob, bvar, scale, lon, lat)

        dm = ec.spread_skill(members, obs)
        dc = ec.coverage(members, obs, interval=0.9)
        print(f"  {'DOMAIN (all sites)':42s} n={members.shape[1]:5d}  "
              f"ratio={dm['ratio']:6.3f}  cov90={dc['coverage']:6.3f}\n")

        rows = stratified_calibration(members, obs, lc, LANDCOVER_LABELS)
        print(f"  {'land cover class':42s} {'n':>5s}  {'ratio':>6s}  {'cov90':>6s}")
        for row in rows:
            label = f"{row['group']} {row['label']}"
            if np.isnan(row["ratio"]):
                print(f"  {label:42s} {row['n']:5d}  (too few sites)")
            else:
                print(f"  {label:42s} {row['n']:5d}  {row['ratio']:6.3f}  {row['cov90']:6.3f}")
        print()


if __name__ == "__main__":
    main()
