"""
Tests for the stratified_calibration engine in regional_diagnostics.py, using
synthetic data with known per-group calibration. These tests do not touch any
real data files: they build ensembles and observations directly and check that
the engine reports the right calibration for each group.
"""

import numpy as np
from regional_diagnostics import stratified_calibration


def make_group(kind, n_members, n_sites, seed):
    """Build (members, obs) for one group with a known calibration."""
    rng = np.random.default_rng(seed)
    truth = rng.normal(0, 1, size=n_sites)
    obs = truth + rng.normal(0, 1, size=n_sites)
    scale = {"calibrated": 1.0, "overconfident": 0.3, "underconfident": 3.0}[kind]
    members = rng.normal(loc=truth, scale=scale, size=(n_members, n_sites))
    return members, obs


def test_separates_groups_by_calibration():
    """
    Two groups, one calibrated and one overconfident, concatenated. The engine
    must report a near-1 ratio for the calibrated group and a much smaller ratio
    for the overconfident one, each on its own subset of sites.
    """
    n_members = 40
    m_cal, o_cal = make_group("calibrated", n_members, 4000, seed=1)
    m_over, o_over = make_group("overconfident", n_members, 4000, seed=2)

    members = np.concatenate([m_cal, m_over], axis=1)
    obs = np.concatenate([o_cal, o_over])
    groups = np.array([0] * 4000 + [1] * 4000)
    labels = {0: "calibrated group", 1: "overconfident group"}

    rows = stratified_calibration(members, obs, groups, labels)
    by_id = {r["group"]: r for r in rows}

    assert 0.8 < by_id[0]["ratio"] < 1.2, \
        f"calibrated group ratio should be ~1, got {by_id[0]['ratio']:.2f}"
    assert by_id[1]["ratio"] < 0.6, \
        f"overconfident group ratio should be <<1, got {by_id[1]['ratio']:.2f}"
    assert by_id[0]["n"] == 4000 and by_id[1]["n"] == 4000
    print(f"separates groups: calibrated ratio {by_id[0]['ratio']:.2f}, "
          f"overconfident ratio {by_id[1]['ratio']:.2f}  OK")


def test_coverage_per_group():
    """Coverage should be near 0.9 for a calibrated group, low for overconfident."""
    n_members = 60
    m_cal, o_cal = make_group("calibrated", n_members, 5000, seed=3)
    m_over, o_over = make_group("overconfident", n_members, 5000, seed=4)
    members = np.concatenate([m_cal, m_over], axis=1)
    obs = np.concatenate([o_cal, o_over])
    groups = np.array([0] * 5000 + [1] * 5000)
    labels = {0: "cal", 1: "over"}

    rows = stratified_calibration(members, obs, groups, labels)
    by_id = {r["group"]: r for r in rows}
    assert abs(by_id[0]["cov90"] - 0.9) < 0.06, \
        f"calibrated coverage should be ~0.9, got {by_id[0]['cov90']:.3f}"
    assert by_id[1]["cov90"] < 0.6, \
        f"overconfident coverage should be well below 0.9, got {by_id[1]['cov90']:.3f}"
    print(f"coverage per group: calibrated {by_id[0]['cov90']:.2f}, "
          f"overconfident {by_id[1]['cov90']:.2f}  OK")


def test_small_group_is_skipped():
    """A group with fewer than ten sites is returned with NaN metrics, not an error."""
    members, obs = make_group("calibrated", 30, 5, seed=5)  # only 5 sites
    groups = np.zeros(5, dtype=int)
    labels = {0: "tiny", 1: "empty"}
    rows = stratified_calibration(members, obs, groups, labels)
    by_id = {r["group"]: r for r in rows}
    assert np.isnan(by_id[0]["ratio"]), "group with <10 sites should be NaN"
    assert by_id[0]["n"] == 5
    assert np.isnan(by_id[1]["ratio"]) and by_id[1]["n"] == 0, "empty group should be NaN/0"
    print("small/empty groups skipped with NaN, no error  OK")


def test_all_groups_present_in_output():
    """Every label id appears in the output, in sorted order."""
    members, obs = make_group("calibrated", 20, 3000, seed=6)
    groups = np.array(([0] * 1000 + [1] * 1000 + [2] * 1000))
    labels = {0: "a", 1: "b", 2: "c"}
    rows = stratified_calibration(members, obs, groups, labels)
    assert [r["group"] for r in rows] == [0, 1, 2], "all groups, sorted"
    assert [r["label"] for r in rows] == ["a", "b", "c"]
    print("all groups present and sorted in output  OK")


if __name__ == "__main__":
    test_separates_groups_by_calibration()
    test_coverage_per_group()
    test_small_group_is_skipped()
    test_all_groups_present_in_output()
    print("\nAll regional diagnostics tests passed.")
