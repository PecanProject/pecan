"""
Ensemble calibration diagnostics for PEcAn benchmarking.

The multi-model comparison in the ILAMB benchmarking evaluates ensemble MEANS:
is the average field accurate? These diagnostics ask a different question, is
the ensemble SPREAD the right size? An ensemble can have an accurate mean yet
be overconfident (spread too narrow for its error) or underconfident (spread
too wide).

Four diagnostics are provided:

  rank_histogram   For each grid cell, the rank of the observation among the
                   sorted ensemble members. Aggregated over cells, a flat
                   histogram indicates calibration; a U shape indicates the
                   ensemble is overconfident (truth falls outside the members
                   too often); a dome indicates underconfidence.

  spread_skill     Ensemble spread (standard deviation across members) versus
                   ensemble error (absolute deviation of the mean from the
                   observation). For a well-calibrated ensemble these are
                   comparable in magnitude; spread much smaller than error
                   indicates overconfidence.

  coverage         Fraction of grid cells where the observation falls within
                   the ensemble range (min to max, or a central interval).
                   Reported against the value expected for a calibrated
                   ensemble of the same size.

  reliability      For an event defined by a threshold, the ensemble forecast
                   probability (fraction of members exceeding it) binned
                   against the observed frequency of the event. A calibrated
                   ensemble lies on the diagonal.

A stochastic-member ensemble (such as a PEcAn data-assimilation reanalysis) is
a true ensemble, so these diagnostics apply to it directly. A multi-model
ensemble (different models, not draws from one distribution) can be scored the
same way, but the diagnostics then test whether that inter-model spread behaves
like a calibrated uncertainty distribution, which is a distinct and largely
untested question.

All functions take numpy arrays. Members are stacked on the first axis:
`members` has shape (n_members, ...) and `obs` has shape (...), matching the
trailing dimensions. NaNs (missing cells) are handled by masking to cells
where the observation and all members are finite.
"""

import numpy as np


def _valid_mask(members, obs):
    """Cells where obs and every member are finite."""
    return np.isfinite(obs) & np.all(np.isfinite(members), axis=0)


def rank_histogram(members, obs, n_bins=None):
    """
    Rank of the observation within the ensemble, per valid cell.

    Returns (counts, edges): `counts` has length n_members + 1 (the number of
    possible ranks), giving how many observations fell at each rank. Ties are
    broken by adding a small uniform jitter, the standard treatment so ties do
    not artificially pile up at one rank. The jitter uses a fixed seed so the
    result is reproducible.
    """
    members = np.asarray(members, dtype=float)
    obs = np.asarray(obs, dtype=float)
    n = members.shape[0]
    mask = _valid_mask(members, obs)

    m = members[:, mask]
    o = obs[mask]

    rng = np.random.default_rng(0)
    jitter = rng.uniform(-1e-9, 1e-9, size=m.shape)
    mj = m + jitter
    oj = o + rng.uniform(-1e-9, 1e-9, size=o.shape)
    ranks = (mj < oj).sum(axis=0)

    counts = np.bincount(ranks, minlength=n + 1)
    edges = np.arange(n + 2)
    return counts, edges


def spread_skill(members, obs):
    """
    Ensemble spread versus error.

    Returns a dict with:
      spread     mean over cells of the ensemble standard deviation
      rmse       root mean square error of the ensemble mean vs obs
      ratio      spread / rmse (near 1 for a well-calibrated ensemble;
                 much less than 1 indicates overconfidence)
    The spread is scaled by sqrt((n+1)/n) so it is comparable to the error of
    the mean for a finite ensemble.
    """
    members = np.asarray(members, dtype=float)
    obs = np.asarray(obs, dtype=float)
    n = members.shape[0]
    mask = _valid_mask(members, obs)

    m = members[:, mask]
    o = obs[mask]
    mean = m.mean(axis=0)
    std = m.std(axis=0, ddof=1)

    spread = float(np.sqrt((n + 1) / n) * std.mean())
    rmse = float(np.sqrt(np.mean((mean - o) ** 2)))
    ratio = spread / rmse if rmse > 0 else float("nan")
    return {"spread": spread, "rmse": rmse, "ratio": ratio}


def coverage(members, obs, interval=None):
    """
    Fraction of cells where obs falls within the ensemble range.

    interval=None uses the full member min-max. Otherwise pass a central
    fraction (e.g. 0.9) to use that central interval via member quantiles.
    Returns a dict with the observed coverage and, for the full range, the
    value expected for a calibrated ensemble of this size: (n-1)/(n+1).
    """
    members = np.asarray(members, dtype=float)
    obs = np.asarray(obs, dtype=float)
    n = members.shape[0]
    mask = _valid_mask(members, obs)

    m = members[:, mask]
    o = obs[mask]

    if interval is None:
        lo = m.min(axis=0)
        hi = m.max(axis=0)
        expected = (n - 1) / (n + 1)
    else:
        q = (1 - interval) / 2
        lo = np.quantile(m, q, axis=0)
        hi = np.quantile(m, 1 - q, axis=0)
        expected = interval

    inside = float(np.mean((o >= lo) & (o <= hi)))
    return {"coverage": inside, "expected": expected, "n_members": n}


def reliability(members, obs, threshold, n_bins=10):
    """
    Reliability of the ensemble probability for an event obs > threshold.

    The forecast probability at each cell is the fraction of members exceeding
    the threshold. Cells are grouped into probability bins; for each bin the
    mean forecast probability and the observed event frequency are returned.
    A calibrated ensemble lies on the diagonal (forecast == observed).

    Returns a dict with bin_prob (mean forecast probability per bin),
    obs_freq (observed frequency per bin), and count (cells per bin).
    """
    members = np.asarray(members, dtype=float)
    obs = np.asarray(obs, dtype=float)
    mask = _valid_mask(members, obs)

    m = members[:, mask]
    o = obs[mask]
    p = (m > threshold).mean(axis=0)
    y = (o > threshold).astype(float)

    edges = np.linspace(0, 1, n_bins + 1)
    idx = np.clip(np.digitize(p, edges) - 1, 0, n_bins - 1)

    bin_prob = np.full(n_bins, np.nan)
    obs_freq = np.full(n_bins, np.nan)
    count = np.zeros(n_bins, dtype=int)
    for b in range(n_bins):
        sel = idx == b
        count[b] = sel.sum()
        if count[b] > 0:
            bin_prob[b] = p[sel].mean()
            obs_freq[b] = y[sel].mean()
    return {"bin_prob": bin_prob, "obs_freq": obs_freq, "count": count}
