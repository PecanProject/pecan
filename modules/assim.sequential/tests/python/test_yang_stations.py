from types import SimpleNamespace

import numpy as np
import pandas as pd
import pytest

from pecan_downscaling.targets.stations import aqua_values


def test_station_composites_follow_coordinates_and_keep_long_gaps(tmp_path):
    sites = pd.DataFrame({"index": [27, 9], "lat": [45, 46], "lon": [-91, -90]})
    dates = pd.date_range("2020-01-09", periods=5, freq="16D", tz="UTC")
    inputs = SimpleNamespace(
        sites=sites,
        paths={"aqua_points": tmp_path},
        periods=pd.DataFrame({"start": [dates[0]], "stop": [dates[-1]]}),
    )
    frames = []
    for site in sites.itertuples():
        frames.append(
            pd.DataFrame(
                {
                    "Latitude": site.lat,
                    "Longitude": site.lon,
                    "Date": dates,
                    "MYD13Q1_061__250m_16_days_EVI": [0.1, -9999, 0.5, 0.7, 0.9],
                    "MYD13Q1_061__250m_16_days_NDVI": [0.2, 2, 2, 2, 0.6],
                }
            )
        )
    frames[1].iloc[:, -2] = 0.3
    path = tmp_path / "sample-MYD13Q1-results.csv"
    frame = pd.concat(frames)
    frame.iloc[::-1].to_csv(path, index=False)
    clock, values = aqua_values(inputs)
    indexes = clock.get_indexer(dates)
    np.testing.assert_allclose(values["EVI"][indexes, 0], [0.1, 0.3, 0.5, 0.7, 0.9])
    np.testing.assert_allclose(values["EVI"][indexes, 1], 0.3)
    np.testing.assert_array_equal(values["EVI_filled"][indexes, 0], [0, 1, 0, 0, 0])
    assert np.isnan(values["NDVI"][indexes[1:4]]).all()
    pd.concat([frame, frame.iloc[:1]]).to_csv(path, index=False)
    with pytest.raises(ValueError, match="duplicate Aqua"):
        aqua_values(inputs)
