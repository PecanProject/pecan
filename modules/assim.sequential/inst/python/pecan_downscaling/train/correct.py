"""Fit period residual forests on complete site windows and nested proxy predictions."""

import numpy as np
import pandas as pd
import xgboost as xgb
from pecan_downscaling.artifacts import receipt
from pecan_downscaling.train.proxy import assembled_path
from pecan_downscaling.train.recipe import BOOSTING, ROUNDS


def prepare(run):
    data = run.data
    expected = data.periods.expected_3hour_steps.to_numpy()
    if np.any(expected > 128):
        raise ValueError("Period exceeds the 128-step residual feature layout")
    counts = np.zeros(data.rows, np.int32)
    for shard in data.shards:
        np.add.at(counts, data.native(shard, "row"), 1)
    rows = np.flatnonzero(
        data.arrays["complete"] & (counts == np.repeat(expected, data.n))
    )
    if not len(rows):
        raise ValueError("No site-period with full three-hour support")
    labels = np.zeros((len(rows), 2), np.float64)
    indices = np.full(data.rows, -1, np.int64)
    indices[rows] = np.arange(len(rows))
    periods = rows // data.n
    starts = (
        (
            pd.to_datetime(data.periods.start_utc, utc=True) - data.start
        ).dt.total_seconds()
        / 10800
    ).to_numpy(np.int64)
    path = run.path("cache/period_features.npy")
    tmp = path.with_suffix(".partial.npy")
    matrix = np.lib.format.open_memmap(
        tmp, mode="w+", dtype="float32", shape=(len(rows), 4220)
    )
    matrix[:, :3968] = np.nan
    matrix[:, 3968:4096] = 0
    categories = np.full((len(rows), 2), -1, np.int32)
    for shard in data.shards:
        native_row = data.native(shard, "row")
        step = data.native(shard, "step")
        native = data.native(shard, "x")
        target = data.native(shard, "y")
        for start in range(0, len(step), 32768):
            batch = np.arange(start, min(start + 32768, len(step)))
            batch = batch[indices[native_row[batch]] >= 0]
            pos = indices[native_row[batch]]
            offset = step[batch] - starts[periods[pos]]
            if np.any((offset < 0) | (offset >= expected[periods[pos]])):
                raise ValueError("Invalid native offset")
            matrix[pos[:, None], offset[:, None] * 31 + np.arange(31)] = native[
                batch, :31
            ]
            matrix[pos, 3968 + offset] = 1
            known = categories[pos, 0] >= 0
            if np.any(categories[pos[known]] != native[batch[known], 31:33]):
                raise ValueError("Changing site-period categories")
            categories[pos] = native[batch, 31:33]
            if not np.array_equal(categories[pos], native[batch, 31:33]):
                raise ValueError("Changing site-period categories")
            np.add.at(labels, pos, target[batch])
    for start in range(0, len(rows), 4096):
        sl = slice(start, start + 4096)
        ids = rows[sl]
        kg, lc = categories[sl].T
        matrix[sl, 4096:] = np.column_stack(
            (
                data.arrays["numeric"][ids],
                np.eye(23)[kg],
                np.eye(8)[lc - 1],
                np.eye(12)[data.arrays["lc"][ids]],
                np.eye(13)[np.clip(data.arrays["years"][ids] - 2012, 0, 12)],
                expected[periods[sl]] / 128,
            )
        )
    matrix.flush()
    del matrix
    tmp.replace(path)
    receipt(path, run.token)
    run.save(
        "cache/periods.npz",
        rows=rows,
        y=(labels / counts[rows, None]).astype(np.float32),
        expected=counts[rows],
    )


class Batches(xgb.DataIter):
    def __init__(self, x, indices, y, device, cache):
        self.x, self.indices, self.y, self.device = x, indices, y, device
        super().__init__(cache_prefix=str(cache), release_data=True)
        self.reset()

    def reset(self):
        self.pos = 0

    def next(self, input_data):
        ix = self.indices[self.pos : self.pos + 32768]
        if not len(ix):
            return False
        values = self.x[ix]
        labels = self.y[ix]
        if self.device == "cuda":
            import cupy as cp

            values, labels = cp.asarray(values), cp.asarray(labels)
        input_data(data=values, label=labels)
        self.pos += len(ix)
        return True


def outputs(o, j):
    return [f"models/residual_o{o}_j{j}_{target}.json" for target in ("NEE", "LE")] + [
        f"predictions/residual_o{o}_j{j}.npz"
    ]


def fit(run, o, j):
    data = run.data
    meta = run.load("cache/periods.npz")
    rows = meta["rows"]
    site = rows % data.n
    outer = data.folds.fold.to_numpy()[site]
    inner = data.folds[f"inner_{o}"].to_numpy()[site]
    fit = np.flatnonzero((outer != o) & ((inner != j) if j else True))
    test = np.flatnonzero((outer != o) & (inner == j) if j else outer == o)
    if not len(fit) or not len(test):
        raise ValueError(f"No complete training or evaluation windows for fold {o}/{j}")
    baseline = run.array(assembled_path(o, j))[rows, :, 0].copy()
    if j:
        baseline[test] = run.array(assembled_path(o))[rows[test], :, 0]
    if not np.isfinite(baseline[np.r_[fit, test]]).all():
        raise ValueError("Missing cross-fitted proxy baseline")
    residual = meta["y"] - baseline
    features = run.array("cache/period_features.npy")
    device = run.config.device
    params = dict(
        BOOSTING,
        device=device,
        nthread=run.config.cpu_threads,
        objective="reg:squarederror",
    )
    pred = baseline[test].copy()
    iterator = Batches(
        features, fit, residual[:, 0], device, run.path(f"tmp/matrix_{o}_{j}")
    )
    matrix = xgb.ExtMemQuantileDMatrix(
        iterator, max_bin=256, nthread=run.config.cpu_threads
    )
    for t, target in enumerate(("NEE", "LE")):
        matrix.set_label(residual[fit, t])
        booster = xgb.train(params, matrix, num_boost_round=ROUNDS)
        path = run.path(outputs(o, j)[t])
        temp = path.with_suffix(".partial.json")
        booster.save_model(temp)
        temp.replace(path)
        receipt(path, run.token)
        for begin in range(0, len(test), 32768):
            part = slice(begin, begin + 32768)
            x = features[test[part]]
            if device == "cuda":
                import cupy as cp

                pred[part, t] += booster.inplace_predict(cp.asarray(x)).get()
            else:
                pred[part, t] += booster.inplace_predict(x)
    run.save(outputs(o, j)[-1], test=test, pred=pred)
