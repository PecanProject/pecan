import numpy as np
import pandas as pd
import pytest
from pecan_downscaling.artifacts import write
from pecan_downscaling.contracts import NUMERIC
from pecan_downscaling.train.data import Dataset, publish, period_position
from pecan_downscaling.train.splits import align_ids, declare_nested, proxy_keys, sites


def make_dataset(root):
    root.mkdir()
    (root / "panel").mkdir()
    (root / "native").mkdir()
    count = 54
    folds = pd.DataFrame(
        dict(
            index=np.arange(count) * 7 + 11,
            fold=np.tile([1, 2, 3], count // 3),
            LC=np.ones(count, int),
            is_tower=np.zeros(count, bool),
            lat=np.linspace(40, 45, count),
            lon=np.linspace(-100, -90, count),
        )
    )
    for outer in (1, 2, 3):
        folds[f"inner_{outer}"] = 0
        fit = folds.fold != outer
        folds.loc[fit, f"inner_{outer}"] = np.tile([1, 2, 3], int(fit.sum()) // 3)
    folds = declare_nested(folds)
    folds.to_csv(root / "folds.csv", index=False)
    pd.DataFrame(
        dict(
            period_id=[7, 8],
            period_key=["2020-02-28", "2020-03-01"],
            start_utc=["2020-02-28T00:00:00Z", "2020-03-01T00:00:00Z"],
            end_date=["2020-02-29", "2020-03-01"],
            expected_3hour_steps=[16, 8],
        )
    ).to_csv(root / "periods.csv", index=False)
    rng = np.random.default_rng(7)
    arrays = dict(
        numeric=rng.normal(size=(count * 2, len(NUMERIC))).astype(np.float32),
        lc=np.arange(count * 2, dtype=np.int64) % 12,
        years=np.full(count * 2, 2020),
        complete=np.ones(count * 2, bool),
        target_mean=rng.normal(size=(count * 2, 2)).astype(np.float32),
        site_ids=folds["index"].to_numpy(),
    )
    for key, value in arrays.items():
        np.save(root / f"panel/{key}.npy", value)
    row = np.arange(count * 2, dtype=np.int32)
    step = np.repeat([0, 16], count).astype(np.int32)
    x = rng.normal(size=(count * 2, 33)).astype(np.float32)
    x[:, 31], x[:, 32] = 0, 1
    native = dict(
        x=x,
        y=arrays["target_mean"] + 0.5,
        row=row,
        step=step,
        route=np.zeros((count * 2, 2, 3), np.float32),
    )
    native["route"][:, :, 1] = 1
    for key, value in native.items():
        np.save(root / f"native/sample.{key}.npy", value)
    write(root / "native/sample.json", dict(name="sample", rows=len(row)))
    publish(root, ["sample"], dict(synthetic=True), "2020-02-28T00:00:00Z")
    return root


def test_all_39_splits_and_reordered_ids(training_data):
    data = Dataset(training_data)
    assert len(proxy_keys()) == 39
    for outer, inner, nested in proxy_keys():
        fit, held = sites(data.folds, outer, inner, nested)
        assert not set(fit["index"]) & set(held["index"])
        assert not (fit.fold == outer).any()
        if nested:
            assert not (fit[f"inner_{outer}"] == inner).any()
            assert not (held[f"inner_{outer}"] == inner).any()
    np.testing.assert_array_equal(align_ids([11, 4], [4, 11], "ids"), [1, 0])


@pytest.mark.parametrize(
    "source,requested", [([1, 1], [1]), ([1, 2], [3]), ([1, 2], [1, 1])]
)
def test_bad_ids(source, requested):
    with pytest.raises(ValueError):
        align_ids(source, requested, "fixture")


def test_missing_declared_shard_fails(training_data):
    (training_data / "native/sample.y.npy").rename(training_data / "missing-y")
    with pytest.raises(ValueError, match="sample.y"):
        Dataset(training_data)


def test_failed_republish_keeps_manifest(training_data):
    path = training_data / "manifest.json"
    before = path.read_bytes(), path.stat().st_mtime_ns
    values = np.load(training_data / "native/sample.row.npy")
    values[0] = -1
    np.save(training_data / "native/sample.row.npy", values)
    with pytest.raises(ValueError, match="Out-of-range"):
        publish(training_data, ["sample"], {}, "2020-02-28T00:00:00Z")
    assert (path.read_bytes(), path.stat().st_mtime_ns) == before


def test_leap_boundary(training_data):
    data = Dataset(training_data)
    positions, valid = period_position(
        ["2020-02-29T21:00Z", "2020-03-01T00:00Z", "2020-03-02T00:00Z"], data.periods
    )
    np.testing.assert_array_equal(positions, [0, 1, 1])
    np.testing.assert_array_equal(valid, [True, True, False])
