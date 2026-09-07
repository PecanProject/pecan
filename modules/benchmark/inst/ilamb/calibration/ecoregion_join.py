"""
Assign each site an EPA/CEC North American ecoregion (Level 1 and Level 2) by
point-in-polygon, then run the same stratified calibration engine on the
ecoregion grouping.

The ecoregion polygons and projection follow PEcAn's EPA_ecoregion_finder. The
shapefiles are the CEC "Ecoregions of North America" Level 1 and Level 2 files,
which cover all of North America (the CONUS-only versions bundled with some
tools drop the boreal and Arctic north). They are a free download from the EPA:

    https://www.epa.gov/eco-research/ecoregions-north-america
    (na_cec_eco_l1.zip and na_cec_eco_l2.zip)

Unzip them and point ECO_DIR below at the folder containing
NA_CEC_Eco_Level1.shp and NA_CEC_Eco_Level2.shp. Requires geopandas.

The WATER category is treated as unmatched. Sites that fall outside every
polygon (open ocean, a few coastal points) are also left unmatched.
"""

import os
import numpy as np
import geopandas as gpd
from shapely.geometry import Point

import ensemble_calibration as ec
from regional_diagnostics import (
    load_sites, sample_ensemble, sample_benchmark,
    stratified_calibration, VARIABLES, VAR_LABEL,
)

# Folder holding NA_CEC_Eco_Level1.shp and NA_CEC_Eco_Level2.shp (adapt this).
ECO_DIR = "ecoregions_na"
LEVELS = {
    "L1": ("NA_CEC_Eco_Level1.shp", "NA_L1NAME"),
    "L2": ("NA_CEC_Eco_Level2.shp", "NA_L2NAME"),
}
EXCLUDE = {"WATER"}  # not a real ecoregion


def isnull(arr):
    """Elementwise None-or-NaN test for an object array of region names."""
    return np.array([(x is None) or (isinstance(x, float) and np.isnan(x))
                     for x in arr])


def assign_ecoregions(lon, lat):
    """Return {level: name_array} assigning each site an ecoregion by polygon."""
    pts = gpd.GeoDataFrame(geometry=[Point(x, y) for x, y in zip(lon, lat)],
                           crs="EPSG:4326")
    out = {}
    for level, (fname, name_col) in LEVELS.items():
        poly = gpd.read_file(os.path.join(ECO_DIR, fname))
        # the shapefile carries its own equal-area CRS; reproject to lon/lat
        poly = poly.to_crs("EPSG:4326")[[name_col, "geometry"]].copy()
        joined = gpd.sjoin(pts, poly, how="left", predicate="within")
        # a point can fall in overlapping polygon parts; keep the first per point
        joined = joined[~joined.index.duplicated(keep="first")].sort_index()
        names = joined[name_col].values.astype(object)
        names = np.array([None if (n in EXCLUDE) else n for n in names],
                         dtype=object)
        out[level] = names
    return out


def _groups_from_names(names):
    """Map region names to integer ids and a label dict for the engine."""
    uniq = sorted(set(names[~isnull(names)]))
    name_to_id = {n: i for i, n in enumerate(uniq)}
    labels = {i: n for n, i in name_to_id.items()}
    groups = np.array([name_to_id.get(n, -1) for n in names])
    return groups, labels


def run_level(level_name, names, ens_cache, min_sites=30):
    """Print the stratified calibration by ecoregion at one level."""
    groups, labels = _groups_from_names(names)
    for var, (map_var, bglob, bvar, scale) in VARIABLES.items():
        members, obs = ens_cache[var]
        print(f"\n{VAR_LABEL[var]} ({var}) by {level_name} ecoregion")
        print(f"  {'ecoregion':45s} {'n':>5s}  {'ratio':>6s}  {'cov90':>6s}")
        rows = sorted(stratified_calibration(members, obs, groups, labels),
                      key=lambda r: -r["n"])
        for row in rows:
            if row["n"] < min_sites:
                continue
            lab = row["label"][:44]
            if np.isnan(row["ratio"]):
                print(f"  {lab:45s} {row['n']:5d}  (too few)")
            else:
                print(f"  {lab:45s} {row['n']:5d}  {row['ratio']:6.3f}  {row['cov90']:6.3f}")


def main():
    lon, lat, lc = load_sites()
    eco = assign_ecoregions(lon, lat)
    l1, l2 = eco["L1"], eco["L2"]
    n_l1 = len(set(l1[~isnull(l1)]))
    n_l2 = len(set(l2[~isnull(l2)]))
    unmatched = int(isnull(l1).sum())
    print(f"assigned ecoregions to {len(lon)} sites: "
          f"{n_l1} L1 regions, {n_l2} L2 regions, {unmatched} sites unmatched\n")

    ens_cache = {}
    for var, (map_var, bglob, bvar, scale) in VARIABLES.items():
        ens_cache[var] = (sample_ensemble(map_var, lon, lat),
                          sample_benchmark(bglob, bvar, scale, lon, lat))

    print("=" * 72); print("LEVEL 1 ECOREGIONS"); print("=" * 72)
    run_level("L1", l1, ens_cache)
    print("\n" + "=" * 72); print("LEVEL 2 ECOREGIONS"); print("=" * 72)
    run_level("L2", l2, ens_cache)


if __name__ == "__main__":
    main()
