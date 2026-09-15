"""
Score the PEcAn ensemble calibration against the ESACCI biomass benchmark, and
test whether the overconfidence finding survives observation error.

Two parts:

1. Scoring: sample the downscaled biomass ensemble at the assimilation sites and
   compute the calibration diagnostics against ESACCI (and, for comparison, the
   XuSaatchi benchmark). If the ensemble is overconfident against both an older
   and a recent independent product, the finding is cross-validated.

2. Observation-error propagation: standard calibration treats the observation as
   perfect. This adds the observation error to the ensemble spread before
   computing coverage and the spread-skill ratio, so the observation only counts
   as outside the ensemble when it is beyond the ensemble range widened by its
   own uncertainty. Two obs-error estimates are used: ESACCI's per-pixel standard
   deviation, and the disagreement between the XuSaatchi and ESACCI products (a
   larger estimate, since two independent products disagreeing is a direct
   measure of how uncertain biomass observations really are).

Reuses ensemble_calibration.py for the base diagnostics. The obs-error functions
here (coverage_with_obs_error, spread_skill_with_obs_error) are general and apply
to any benchmark that carries an uncertainty estimate.

NOTE ON PATHS: the site shapefile, the downscaled-map layout, and the benchmark
locations reflect one particular setup and are illustrative; adapt them to your
own data. The ensemble maps are carbon in kg m-2; the benchmarks are Mg C/ha and
are scaled by 0.1 to kg m-2 at sampling time.
"""

import os
import glob
import numpy as np

# ensemble_calibration.py is expected alongside this module (see README).
import ensemble_calibration as ec

SDA_DIR = "/projectnb/dietzelab/dongchen/anchorSites/NA_runs/SDA_8k_site"
SHAPEFILE = f"{SDA_DIR}/shapefile/pts.shp"
MAPS_DIR = f"{SDA_DIR}/downscale_maps_analysis_lc_ts_noGEDI_rf"
YEAR = 2015  # the year for which downscaled ensemble maps are available

# ESACCI time index for a given year in the converted benchmark (2015 = 0).
ESACCI_YEARS = [2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024]


def _ilamb_root():
    return os.environ.get("ILAMB_ROOT", "")


def load_sites():
    import fiona
    lon, lat = [], []
    with fiona.open(SHAPEFILE) as src:
        for r in src:
            x, y = r["geometry"]["coordinates"]
            lon.append(x)
            lat.append(y)
    return np.array(lon), np.array(lat)


def sample_ensemble(lon, lat):
    import rasterio
    files = sorted(glob.glob(
        f"{MAPS_DIR}/AbvGrndWood_{YEAR}/ensemble_*_{YEAR}_AbvGrndWood.tiff"))
    if not files:
        raise FileNotFoundError(f"no ensemble maps in {MAPS_DIR}")
    members = []
    for f in files:
        with rasterio.open(f) as r:
            v = np.array([s[0] for s in r.sample(list(zip(lon, lat)))], dtype=float)
            if r.nodata is not None:
                v[v == r.nodata] = np.nan
        members.append(v)
    return np.vstack(members)


def sample_nc(path, var, lon, lat, time_index=None, scale=1.0):
    import xarray as xr
    a = xr.open_dataset(path, decode_times=False)[var]
    ren = {}
    for c in a.coords:
        cl = str(c).lower()
        if cl in ("latitude", "y"): ren[c] = "lat"
        if cl in ("longitude", "x"): ren[c] = "lon"
    a = a.rename(ren)
    if "time" in a.dims:
        a = a.isel(time=time_index) if time_index is not None else a.mean("time")
    xa = xr.DataArray(lon, dims="site")
    ya = xr.DataArray(lat, dims="site")
    return a.sel(lat=ya, lon=xa, method="nearest").values * scale


# ----------------------------------------------------------------------------
# Observation-error aware diagnostics (general, reusable)
# ----------------------------------------------------------------------------

_Z90 = 1.6448536269514722  # standard normal quantile for the central 90 percent


def coverage_with_obs_error(members, obs, obs_err, interval=0.9, z=_Z90):
    """
    Fraction of observations within the ensemble interval widened by obs error.

    The ensemble percentile bounds are expanded by z * obs_err on each side, so an
    observation counts as outside only if it lies beyond the ensemble range plus
    its own uncertainty. With obs_err = 0 this reduces to the ordinary coverage.
    """
    lo_q = (1 - interval) / 2 * 100
    hi_q = (1 + interval) / 2 * 100
    lo = np.nanpercentile(members, lo_q, axis=0) - z * obs_err
    hi = np.nanpercentile(members, hi_q, axis=0) + z * obs_err
    inside = (obs >= lo) & (obs <= hi)
    valid = np.isfinite(obs) & np.isfinite(obs_err)
    return float(np.mean(inside[valid]))


def spread_skill_with_obs_error(members, obs, obs_err):
    """
    Spread-to-error ratio with observation error added to the ensemble spread in
    quadrature. Near 1 when calibrated; well below 1 when overconfident even after
    accounting for observation uncertainty.
    """
    ens_var = np.nanvar(members, axis=0, ddof=1)
    total_spread = np.sqrt(ens_var + obs_err ** 2)
    ens_mean = np.nanmean(members, axis=0)
    err = np.abs(ens_mean - obs)
    valid = np.isfinite(obs) & np.isfinite(obs_err) & np.isfinite(err)
    rms = np.sqrt(np.nanmean(err[valid] ** 2))
    return float(np.nanmean(total_spread[valid]) / rms)


# ----------------------------------------------------------------------------
# Driver
# ----------------------------------------------------------------------------

def main():
    lon, lat = load_sites()
    print(f"sampling ensemble at {len(lon)} sites...")
    members = sample_ensemble(lon, lat)

    esacci = "ESACCI_biomass_0.5deg.nc"
    i2020 = ESACCI_YEARS.index(2020)
    i2024 = ESACCI_YEARS.index(2024)

    xu = sample_nc(f"{_ilamb_root()}/DATA/biomass/XuSaatchi2021/XuSaatchi.nc",
                   "biomass", lon, lat, None, 0.1)
    obs = sample_nc(esacci, "biomass", lon, lat, i2020, 0.1)
    obs2024 = sample_nc(esacci, "biomass", lon, lat, i2024, 0.1)
    obs_sd = sample_nc(esacci, "biomass_sd", lon, lat, i2020, 0.1)
    disagree = np.abs(xu - obs)

    print("\nCalibration vs each biomass benchmark:")
    for name, o in [("XuSaatchi (~2010)", xu),
                    ("ESACCI 2020", obs),
                    ("ESACCI 2024", obs2024)]:
        ss = ec.spread_skill(members, o)
        cov = ec.coverage(members, o, interval=0.9)
        print(f"  {name:20s}  ratio={ss['ratio']:.3f}  cov90={cov['coverage']:.3f}")

    print(f"\nObs-error estimates: ESACCI SD median {np.nanmedian(obs_sd):.3f}, "
          f"XuSaatchi-ESACCI disagreement median {np.nanmedian(disagree):.3f} kg/m2")

    print("\n90% coverage (target 0.9), observation error added to the ensemble band:")
    print(f"  raw:                            {ec.coverage(members, obs, 0.9)['coverage']:.3f}")
    print(f"  + ESACCI per-pixel SD:          {coverage_with_obs_error(members, obs, obs_sd):.3f}")
    print(f"  + XuSaatchi-ESACCI disagreement:{coverage_with_obs_error(members, obs, disagree):.3f}")

    print("\nSpread-skill ratio (target 1.0), obs error added to the spread:")
    print(f"  raw:                            {ec.spread_skill(members, obs)['ratio']:.3f}")
    print(f"  + ESACCI per-pixel SD:          {spread_skill_with_obs_error(members, obs, obs_sd):.3f}")
    print(f"  + XuSaatchi-ESACCI disagreement:{spread_skill_with_obs_error(members, obs, disagree):.3f}")


if __name__ == "__main__":
    main()
