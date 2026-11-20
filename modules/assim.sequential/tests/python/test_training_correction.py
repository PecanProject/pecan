import pytest
from pecan_downscaling.train.state import Run
from pecan_downscaling.train.correct import prepare
from pecan_downscaling.train.configuration import Config


def test_partial_periods_excluded(training_data, tmp_path):
    run = Run(Config(training_data, tmp_path / "run"))
    with pytest.raises(ValueError, match="full three-hour support"):
        prepare(run)


def test_dense_layout_and_targets_use_full_actual_window(training_data, tmp_path):
    import numpy as np
    from pecan_downscaling.artifacts import write
    from pecan_downscaling.train.data import publish

    # The first period has 16 steps. The second deliberately has partial support.
    base = training_data / "native"
    rows = np.load(base / "sample.row.npy")
    n = len(rows) // 2
    selected = np.r_[np.tile(np.arange(n), 16), np.arange(n, 2 * n)]
    for key in ("x", "y", "row", "step", "route"):
        path = base / f"sample.{key}.npy"
        a = np.load(path)[selected]
        if key == "step":
            a[: 16 * n] = np.repeat(np.arange(16), n)
        np.save(path, a)
    write(base / "sample.json", dict(name="sample", rows=len(selected)))
    (training_data / "manifest.json").rename(tmp_path / "initial-manifest.json")
    publish(training_data, ["sample"], {}, "2020-02-28T00:00:00Z")
    run = Run(Config(training_data, tmp_path / "run"))
    prepare(run)
    meta = run.load("cache/periods.npz")
    x = run.array("cache/period_features.npy")
    assert x.shape == (n, 4220)
    np.testing.assert_array_equal(meta["rows"], np.arange(n))
    np.testing.assert_array_equal(meta["expected"], np.full(n, 16))
    np.testing.assert_array_equal(meta["y"], np.load(base / "sample.y.npy")[:n])
    np.testing.assert_array_equal(x[:, :31], np.load(base / "sample.x.npy")[:n, :31])
    assert np.isnan(x[:, 16 * 31 : 3968]).all()
    assert (x[:, 3968:3984] == 1).all() and (x[:, 3984:4096] == 0).all()
    np.testing.assert_array_equal(x[:, 4096:4163], run.data.arrays["numeric"][:n])
    assert (x[:, -1] == 16 / 128).all()
