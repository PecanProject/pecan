import numpy as np
from pecan_downscaling.train.evaluate import spatial_query


def test_train_side_leave_self_out():
    coords = np.array([[40.0, -100.0], [41.0, -100.0], [42.0, -100.0]])
    cells = np.broadcast_to(np.array([1.0, 2.0, 9.0])[:, None, None], (3, 12, 2))
    model = dict(
        cell=cells, lc=np.ones(3), coords=coords, train_positions=np.array([7, 8, 9])
    )
    bias, _ = spatial_query(model, coords[[2]], np.ones(1), own=[9])
    np.testing.assert_array_equal(bias, np.full((1, 12, 2), 1.5))
