"""Replay up to 512 held-out site-periods per fold using the exported CPU models."""

import argparse
import os
from pathlib import Path
import sys

import numpy as np
import pandas as pd

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "inst/python"))


def check(run):
    # This verifier uses CPU inference, including when run on a GPU host.
    os.environ["CUDA_VISIBLE_DEVICES"] = ""
    import xgboost
    from pecan_downscaling.models.inference import Models
    from pecan_downscaling.train.data import Dataset

    models = Models(run / "model_bundle")
    data = Dataset(run / "prepared_data")
    assert models.manifest["provenance"]["prepared_data_id"] == data.manifest["id"]
    with np.load(run / "cache/periods.npz") as periods:
        rows = periods["rows"]
    matrix = np.load(run / "cache/period_features.npy", mmap_mode="r")
    months = pd.to_datetime(data.periods.period_key).dt.month.to_numpy() - 1
    for fold in (1, 2, 3):
        with np.load(run / f"predictions/residual_o{fold}_j0.npz") as held:
            test, residual_pred = held["test"][:512], held["pred"][:512]
        keys = rows[test]
        site = data.folds.iloc[keys % data.n]
        assert (site.fold == fold).all() and len(test)
        a = data.arrays
        proxy = models.proxy_arrays(
            a["numeric"][keys], a["lc"][keys], a["years"][keys]
        )[fold - 1]
        saved = np.load(run / f"cache/proxy_o{fold}_j0.npy", mmap_mode="r")[keys, :, 0]
        np.testing.assert_allclose(proxy, saved, rtol=2e-5, atol=2e-5)
        predicted = proxy.copy()
        for i, target in enumerate(("NEE", "LE")):
            booster = xgboost.Booster()
            booster.load_model(models.root / f"residual/fold_{fold}_{target}.json")
            booster.set_param(dict(device="cpu", nthread=1))
            predicted[:, i] += booster.inplace_predict(matrix[test])
        np.testing.assert_allclose(predicted, residual_pred, rtol=2e-5, atol=2e-5)
        spatial = models.spatial[fold].query(
            site[["lat", "lon"]].to_numpy(), site.LC.to_numpy()
        )
        corrected = predicted - spatial[months[keys // data.n], np.arange(len(test))]
        with np.load(run / f"predictions/spatial_o{fold}.npz") as held:
            np.testing.assert_array_equal(test, held["test"][: len(test)])
            np.testing.assert_allclose(
                corrected, held["pred"][: len(test)], rtol=2e-5, atol=2e-5
            )
        print(
            f"Fold {fold}: {len(keys)} held-out proxy, residual and spatial predictions verified"
        )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("run", type=Path)
    check(parser.parse_args().run)
