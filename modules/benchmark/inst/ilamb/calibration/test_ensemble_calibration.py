"""
Tests for ensemble_calibration.py using synthetic ensembles with KNOWN
calibration. The whole point: if we construct an ensemble that is overconfident
by design, the diagnostics must report overconfidence, and likewise for
well-calibrated and underconfident cases. These tests do not touch real data.
"""

import numpy as np
import ensemble_calibration as ec


def make_case(kind, n_members=50, n_cells=20000, seed=1):
    """
    Build (members, obs) with a known calibration.

    Truth is drawn from N(0, 1) per cell. obs = truth + N(0, 1), so given truth
    the observation is N(truth, 1). A calibrated ensemble draws members from the
    same conditional law N(truth, 1); then obs is exchangeable with the members
    and the rank histogram is flat. Overconfident/underconfident cases scale the
    member spread down/up from that.
    """
    rng = np.random.default_rng(seed)
    truth = rng.normal(0, 1, size=n_cells)
    obs = truth + rng.normal(0, 1, size=n_cells)
    if kind == "calibrated":
        s = 1.0
    elif kind == "overconfident":
        s = 0.3
    elif kind == "underconfident":
        s = 3.0
    else:
        raise ValueError(kind)
    members = rng.normal(loc=truth, scale=s, size=(n_members, n_cells))
    return members, obs


def test_rank_histogram_shapes():
    m, o = make_case("calibrated")
    counts, _ = ec.rank_histogram(m, o)
    flat = counts / counts.mean()
    assert flat.max() < 1.5, f"calibrated hist not flat: {flat.max():.2f}"

    m, o = make_case("overconfident")
    counts, _ = ec.rank_histogram(m, o)
    ends = counts[0] + counts[-1]
    middle = counts[1:-1].mean() * 2
    assert ends > 5 * middle, f"overconfident not U-shaped: ends={ends} mid={middle:.0f}"

    m, o = make_case("underconfident")
    counts, _ = ec.rank_histogram(m, o)
    ends = (counts[0] + counts[-1]) / 2
    middle = counts[len(counts)//2]
    assert middle > 3 * ends, f"underconfident not dome: mid={middle} ends={ends:.0f}"
    print("rank_histogram: calibrated flat, overconfident U, underconfident dome  OK")


def test_spread_skill_ratio():
    m, o = make_case("calibrated")
    r = ec.spread_skill(m, o)["ratio"]
    assert 0.85 < r < 1.15, f"calibrated ratio should be ~1, got {r:.2f}"

    r_over = ec.spread_skill(*make_case("overconfident"))["ratio"]
    assert r_over < 0.5, f"overconfident ratio should be <<1, got {r_over:.2f}"

    r_under = ec.spread_skill(*make_case("underconfident"))["ratio"]
    assert r_under > 1.8, f"underconfident ratio should be >>1, got {r_under:.2f}"
    print(f"spread_skill: calibrated~1 ({r:.2f}), over<<1 ({r_over:.2f}), under>>1 ({r_under:.2f})  OK")


def test_coverage():
    m, o = make_case("calibrated")
    c = ec.coverage(m, o)
    assert abs(c["coverage"] - c["expected"]) < 0.05, f"coverage {c['coverage']:.3f} vs expected {c['expected']:.3f}"

    # Full min-max coverage is a weak probe with many members; the central
    # interval is the discriminating test for overconfidence.
    mo, oo = make_case("overconfident")
    c_over = ec.coverage(mo, oo, interval=0.9)
    assert c_over["coverage"] < 0.6, f"overconfident central-90 coverage should be well below 0.9, got {c_over['coverage']:.3f}"
    c_cal = ec.coverage(m, o, interval=0.9)
    assert abs(c_cal["coverage"] - 0.9) < 0.05, f"calibrated central-90 should be ~0.9, got {c_cal['coverage']:.3f}"
    print(f"coverage: calibrated full {c['coverage']:.3f} (exp {c['expected']:.3f}), central-90 {c_cal['coverage']:.3f}; overconfident central-90 {c_over['coverage']:.3f}  OK")


def test_reliability_diagonal():
    m, o = make_case("calibrated", n_cells=40000)
    r = ec.reliability(m, o, threshold=0.0, n_bins=10)
    ok = np.isfinite(r["bin_prob"]) & (r["count"] > 50)
    dev = np.abs(r["bin_prob"][ok] - r["obs_freq"][ok])
    assert dev.max() < 0.12, f"reliability off diagonal: max dev {dev.max():.3f}"
    print(f"reliability: calibrated on diagonal (max dev {dev.max():.3f})  OK")


def test_nan_handling():
    m, o = make_case("calibrated", n_cells=5000)
    o = o.copy(); o[:1000] = np.nan
    m = m.copy(); m[:, 1000:1500] = np.nan
    counts, _ = ec.rank_histogram(m, o)
    assert counts.sum() == 3500, f"nan masking wrong: counted {counts.sum()}"
    print("nan_handling: masks missing obs and member cells correctly  OK")


def test_member_count_robustness():
    """
    A calibrated ensemble should score as calibrated regardless of how many
    members it has. This matters because we compare a 100-member PEcAn ensemble
    against ~14-member CMIP6/TRENDY ensembles.

    The bias-corrected spread-skill RATIO is the metric that is fair across
    sizes (it carries the sqrt((n+1)/n) correction), so it should stay near 1
    at every member count. Fixed-interval coverage, by contrast, is NOT directly
    comparable across sizes: a small ensemble cannot resolve a 90 percent central
    interval, so its coverage is legitimately below 0.9 even when calibrated.
    We therefore assert the ratio is stable, and that coverage rises toward 0.9
    as the ensemble grows (rather than asserting a flat target).
    """
    covs = {}
    for n in [8, 14, 25, 50, 100]:
        m, o = make_case("calibrated", n_members=n, seed=n)
        r = ec.spread_skill(m, o)["ratio"]
        assert 0.8 < r < 1.2, f"n={n}: calibrated ratio drifted to {r:.2f}"
        covs[n] = ec.coverage(m, o, interval=0.9)["coverage"]
    # coverage should increase with member count and approach 0.9 for large n
    assert covs[100] > covs[8], "coverage should rise with member count"
    assert abs(covs[100] - 0.9) < 0.05, f"large-n central-90 should be ~0.9, got {covs[100]:.3f}"
    # an overconfident ensemble must read overconfident at every size
    for n in [8, 14, 100]:
        mo, oo = make_case("overconfident", n_members=n, seed=n)
        r = ec.spread_skill(mo, oo)["ratio"]
        assert r < 0.6, f"n={n}: overconfident ratio should stay <<1, got {r:.2f}"
    print(f"member_count_robustness: ratio stable across 8-100 members; coverage rises {covs[8]:.2f}->{covs[100]:.2f} toward 0.9  OK")


def test_partial_member_coverage():
    """
    Real model output has gaps: some cells are missing for some members. The
    diagnostics must run without crashing on ragged data, return finite results,
    and still recover the calibration on the cells that survive masking. We drop
    a modest fraction of (member, cell) entries at random so that most cells
    still retain most members, mirroring realistic partial coverage rather than
    an extreme case that would mask out nearly everything.
    """
    rng = np.random.default_rng(7)
    m, o = make_case("calibrated", n_members=30, n_cells=40000, seed=7)
    m = m.copy()
    # knock out 3 percent of individual (member, cell) entries at random
    mask = rng.random(m.shape) < 0.03
    m[mask] = np.nan
    # must run, be finite, and remain calibrated on surviving cells
    r = ec.spread_skill(m, o)["ratio"]
    assert np.isfinite(r) and 0.8 < r < 1.2, f"ragged calibrated ratio {r:.3f}"
    c = ec.coverage(m, o, interval=0.9)["coverage"]
    assert np.isfinite(c) and 0.82 < c < 0.95, f"ragged coverage out of range: {c:.3f}"
    # also confirm it simply does not crash under heavier, uneven per-member gaps
    m2, o2 = make_case("calibrated", n_members=20, n_cells=20000, seed=11)
    m2 = m2.copy()
    for k in range(m2.shape[0]):
        drop = rng.choice(m2.shape[1], size=m2.shape[1] // 10, replace=False)
        m2[k, drop] = np.nan
    r2 = ec.spread_skill(m2, o2)["ratio"]
    assert np.isfinite(r2), "spread_skill returned non-finite on heavy ragged gaps"
    print(f"partial_member_coverage: finite and calibrated on ragged data (ratio {r:.2f}, cov {c:.2f})  OK")


if __name__ == "__main__":
    test_rank_histogram_shapes()
    test_spread_skill_ratio()
    test_coverage()
    test_reliability_diagonal()
    test_nan_handling()
    test_member_count_robustness()
    test_partial_member_coverage()
    print("\nAll calibration tests passed.")
