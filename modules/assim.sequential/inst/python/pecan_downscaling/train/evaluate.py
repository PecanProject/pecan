import numpy as np, pandas as pd


def spatial_query(model, coords, lc, own=None):
    from sklearn.neighbors import BallTree

    cells = model["cell"]
    finite = np.isfinite(cells)
    global_sum = np.nansum(cells, axis=0)
    global_n = finite.sum(0)
    out = np.full((len(coords), 12, 2), np.nan)
    distance = np.full(len(coords), np.nan)

    def divide(s, n):
        return np.divide(s, n, out=np.full_like(s, np.nan), where=n > 0)

    for code in np.unique(lc):
        ids = np.flatnonzero(model["lc"] == code)
        take = np.flatnonzero(lc == code)
        local_sum = np.nansum(cells[ids], axis=0)
        local_n = finite[ids].sum(0)
        if len(ids):
            dd, jj = BallTree(
                np.deg2rad(model["coords"][ids]), metric="haversine"
            ).query(
                np.deg2rad(coords[take]), k=min(21 if own is not None else 20, len(ids))
            )
        fallback = np.where(
            local_n > 0, divide(local_sum, local_n), divide(global_sum, global_n)
        )
        for offset, i in enumerate(take):
            base = fallback
            if own is not None:
                own_ix = np.flatnonzero(model["train_positions"] == own[i])
                value = cells[own_ix[0]] if len(own_ix) else np.full((12, 2), np.nan)
                nn = np.isfinite(value)
                vv = np.nan_to_num(value)
                local = divide(local_sum - vv, local_n - nn)
                global_ = divide(global_sum - vv, global_n - nn)
                base = np.where(np.isfinite(local), local, global_)
            if len(ids):
                near = ids[jj[offset]]
                dist = dd[offset]
                if own is not None:
                    good = model["train_positions"][near] != own[i]
                    near = near[good]
                    dist = dist[good]
                near = near[:20]
                dist = dist[:20]
                n = np.isfinite(cells[near]).sum(0)
                v = divide(np.nansum(cells[near], axis=0), n)
                out[i] = np.where(n > 0, v, base)
                if len(dist):
                    distance[i] = dist[-1] * 6371.0088
            else:
                out[i] = base
    return out, distance


def spatial(run, o):
    d = run.load("cache/periods.npz")
    f = run.data.folds
    n = run.data.n
    month = (
        pd.to_datetime(run.data.periods.period_key).dt.month.to_numpy()[d["rows"] // n]
        - 1
    )
    outer = run.load(f"predictions/residual_o{o}_j0.npz")
    test = outer["test"]
    fit = np.flatnonzero(f.fold.to_numpy()[d["rows"] % n] != o)
    sites = np.unique(d["rows"][fit] % n)
    lookup = np.full(n, -1, int)
    lookup[sites] = np.arange(len(sites))
    groups = lookup[d["rows"][fit] % n] * 12 + month[fit]
    cross = np.full_like(d["y"], np.nan)
    visited = np.zeros(len(cross), bool)
    for j in (1, 2, 3):
        z = run.load(f"predictions/residual_o{o}_j{j}.npz")
        assert not visited[z["test"]].any()
        cross[z["test"]] = z["pred"]
        visited[z["test"]] = True
    np.testing.assert_array_equal(np.flatnonzero(visited), fit)
    err = cross[fit].astype(float) - d["y"][fit]
    count = np.bincount(
        groups, weights=d["expected"][fit], minlength=len(sites) * 12
    ).reshape(len(sites), 12)
    sums = np.stack(
        [
            np.bincount(
                groups,
                weights=err[:, ti] * d["expected"][fit],
                minlength=len(sites) * 12,
            ).reshape(len(sites), 12)
            for ti in (0, 1)
        ],
        axis=-1,
    )
    cell = np.divide(
        sums,
        count[:, :, None],
        out=np.full_like(sums, np.nan),
        where=count[:, :, None] > 0,
    )
    model = dict(
        cell=cell,
        lc=f.LC.to_numpy()[sites],
        coords=f.iloc[sites][["lat", "lon"]].to_numpy(),
        train_positions=sites,
    )
    loo, _ = spatial_query(model, model["coords"], model["lc"], sites)
    caps = []
    for ti in (0, 1):
        v = np.abs(loo[:, :, ti]).ravel()
        w = count.ravel()
        good = np.isfinite(v) & (w > 0)
        order = np.argsort(v[good])
        vv = v[good][order]
        ww = w[good][order]
        caps.append(
            vv[min(np.searchsorted(np.cumsum(ww), ww.sum() * 0.999), len(vv) - 1)]
        )
    model["caps"] = np.array(caps)
    run.save(f"models/spatial_o{o}.npz", **model)
    held = np.unique(d["rows"][test] % n)
    bias, dist = spatial_query(
        model, f.iloc[held][["lat", "lon"]].to_numpy(), f.LC.to_numpy()[held]
    )
    ix = pd.Index(held).get_indexer(d["rows"][test] % n)
    correction = np.clip(bias[ix, month[test]], -model["caps"], model["caps"])
    assert np.isfinite(correction).all()
    raw = outer["pred"]
    run.save(
        f"predictions/spatial_o{o}.npz",
        test=test,
        pred=(raw - correction).astype(np.float32),
        distance=dist[ix],
    )


def report(run):
    meta = run.load("cache/periods.npz")
    pred = np.full_like(meta["y"], np.nan)
    for o in (1, 2, 3):
        fold = run.load(f"predictions/spatial_o{o}.npz")
        pred[fold["test"]] = fold["pred"]
    if not np.isfinite(pred).all():
        raise ValueError("Missing held-out period predictions")
    error = pred.astype(float) - meta["y"]
    run.json(
        "validation/period_scores.json",
        dict(
            rows=len(pred),
            targets=["NEE", "LE"],
            rmse=np.sqrt(np.mean(error**2, axis=0)),
            mae=np.mean(abs(error), axis=0),
        ),
    )
