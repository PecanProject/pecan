"""
Tests for the ESACCI extension: the year-to-band mapping and carbon conversion in
the converter, and the observation-error aware diagnostics in the scoring module.
These use synthetic arrays and constants, so they need only NumPy and the
calibration module (no ESACCI data files, no rasterio/xarray).
"""

import numpy as np

from convert_esacci_to_ilamb import YEAR_TO_BAND, CARBON_FRACTION, ESACCI_YEARS
from score_esacci import coverage_with_obs_error, spread_skill_with_obs_error


def test_year_to_band_mapping():
    """Band indices are 1-based and skip the absent 2013/2014 years correctly."""
    assert YEAR_TO_BAND[2005] == 1, "first year is band 1"
    assert YEAR_TO_BAND[2015] == 9, "2015 is band 9 (after 8 early years)"
    assert YEAR_TO_BAND[2020] == 14, "2020 is band 14"
    assert YEAR_TO_BAND[2024] == 18, "2024 is the last band, 18"
    assert 2013 not in ESACCI_YEARS and 2014 not in ESACCI_YEARS, "2013/2014 absent"
    assert len(ESACCI_YEARS) == 18
    print("year-to-band mapping correct (2015=9, 2020=14, 2024=18)  OK")


def test_carbon_fraction_is_physical():
    """The carbon fraction is the IPCC woody default, not a fudge factor."""
    assert CARBON_FRACTION == 0.47, "IPCC default woody carbon fraction"
    # a biomass value converts to about half in carbon
    biomass = 200.0
    carbon = biomass * CARBON_FRACTION
    assert 90 < carbon < 100, f"200 Mg/ha biomass -> ~94 Mg C/ha, got {carbon}"
    print(f"carbon fraction physical: 200 Mg/ha biomass -> {carbon:.0f} Mg C/ha  OK")


def _synthetic(kind, n_members, n_sites, seed):
    rng = np.random.default_rng(seed)
    truth = rng.normal(0, 1, size=n_sites)
    obs = truth + rng.normal(0, 1, size=n_sites)
    scale = {"calibrated": 1.0, "overconfident": 0.3}[kind]
    members = rng.normal(loc=truth, scale=scale, size=(n_members, n_sites))
    return members, obs


def test_obs_error_zero_matches_plain_coverage():
    """With zero obs error, coverage_with_obs_error equals ordinary coverage."""
    members, obs = _synthetic("calibrated", 50, 4000, seed=1)
    zero = np.zeros_like(obs)
    cov_plain = coverage_with_obs_error(members, obs, zero, interval=0.9)
    # a calibrated ensemble should have coverage near 0.9
    assert 0.84 < cov_plain < 0.96, f"calibrated coverage ~0.9, got {cov_plain:.3f}"
    print(f"obs-error=0 gives plain coverage {cov_plain:.3f} (~0.9)  OK")


def test_obs_error_widens_coverage():
    """Adding obs error can only increase (never decrease) coverage."""
    members, obs = _synthetic("overconfident", 50, 4000, seed=2)
    zero = np.zeros_like(obs)
    small = np.full_like(obs, 0.2)
    large = np.full_like(obs, 2.0)
    c0 = coverage_with_obs_error(members, obs, zero)
    c1 = coverage_with_obs_error(members, obs, small)
    c2 = coverage_with_obs_error(members, obs, large)
    assert c0 <= c1 <= c2, f"coverage should be monotonic in obs error: {c0},{c1},{c2}"
    assert c2 > c0, "large obs error should raise coverage above the raw value"
    print(f"obs error widens coverage: {c0:.3f} -> {c1:.3f} -> {c2:.3f}  OK")


def test_overconfident_survives_small_obs_error():
    """
    The key property behind the finding: an overconfident ensemble stays
    overconfident when the obs error is small relative to the model-obs gap.
    """
    members, obs = _synthetic("overconfident", 60, 5000, seed=3)
    # small obs error (like ESACCI's per-pixel SD relative to the gap)
    small = np.full_like(obs, 0.1)
    r_raw = spread_skill_with_obs_error(members, obs, np.zeros_like(obs))
    r_small = spread_skill_with_obs_error(members, obs, small)
    assert r_raw < 0.6, f"overconfident ratio should be well below 1, got {r_raw:.3f}"
    assert r_small < 0.7, f"small obs error should not rescue calibration, got {r_small:.3f}"
    print(f"overconfidence survives small obs error: ratio {r_raw:.3f} -> {r_small:.3f}  OK")


if __name__ == "__main__":
    test_year_to_band_mapping()
    test_carbon_fraction_is_physical()
    test_obs_error_zero_matches_plain_coverage()
    test_obs_error_widens_coverage()
    test_overconfident_survives_small_obs_error()
    print("\nAll ESACCI extension tests passed.")
