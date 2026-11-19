import numpy as np
from pecan_downscaling.predict.products import array_for


def test_target_and_band_order():
    folds = np.array([[[1.0, 4.0]], [[3.0, 8.0]], [[5.0, 12.0]]], np.float32)
    z = dict(
        land=np.array([1, 2]),
        reason_ids=np.array([1, 2]),
        reason=np.array([0, 2]),
        pixel=np.array([1]),
        fold_mean=folds,
    )
    tile = dict(row=0, col=0, height=1, width=3)
    bands = array_for(z, tile, 3, 1)
    np.testing.assert_array_equal(
        bands[:, 0, 1], [8.0, folds[:, 0, 1].std(), 4.0, 8.0, 12.0, 1.0, 0.0]
    )
    assert np.isnan(bands[:, 0, 0]).all()
    assert np.isnan(bands[:5, 0, 2]).all() and tuple(bands[5:, 0, 2]) == (0, 2)
