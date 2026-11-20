"""Boundary semantics of the actual environmental readers."""

import numpy as np
import pandas as pd
import pytest
import rasterio
from scipy.io import netcdf_file
from pecan_downscaling.predict.inputs import Sources, short_gaps_matrix
from pecan_downscaling.artifacts import write


@pytest.fixture
def sources(tmp_path):
    legend = tmp_path / "legend.csv"
    pd.DataFrame({"code": [1], "KGC": ["Af"]}).to_csv(legend, index=False)
    ledger = tmp_path / "ledger.json"
    write(ledger, dict(dates=[], sources=[]))
    reader = Sources(dict(era=tmp_path, kgc_legend=legend, aqua_ledger=ledger))
    yield reader
    reader.close()


def test_window_sampler_nodata_and_bounds(tmp_path, sources):
    path = tmp_path / "grid.tif"
    with rasterio.open(
        path,
        "w",
        driver="GTiff",
        width=2,
        height=2,
        count=2,
        dtype="float32",
        crs="EPSG:4326",
        transform=rasterio.transform.from_origin(0, 2, 1, 1),
        nodata=-999,
    ) as grid:
        grid.write(np.array([[[1, 2], [3, -999]], [[5, 6], [7, 8]]], np.float32))
    points = pd.DataFrame(dict(index=[1, 2, 3], lon=[0.5, 1.5, 3], lat=[1.5, 0.5, 1.5]))
    values = sources.sample(path, points, [1, 2])
    np.testing.assert_array_equal(
        values, np.array([[1, 5], [np.nan, 8], [np.nan, np.nan]])
    )


def test_era_accumulation_crosses_year(tmp_path, sources):
    for year, time, value in [(2019, "2019-12-31 21:00", 10), (2020, "2020-01-01", 20)]:
        with netcdf_file(tmp_path / f"ERA5_{year}.nc", "w") as source:
            for name, size in [
                ("time", 1),
                ("member", 2),
                ("latitude", 2),
                ("longitude", 2),
            ]:
                source.createDimension(name, size)
            for name, vals in [
                (
                    "time",
                    [
                        (
                            pd.Timestamp(time, tz="UTC")
                            - pd.Timestamp("1900-01-01", tz="UTC")
                        ).total_seconds()
                        / 3600
                    ],
                ),
                ("latitude", [1, 0]),
                ("longitude", [0, 1]),
            ]:
                source.createVariable(name, "d", (name,))[:] = vals
            for name in ("t2m", "sp", "d2m", "tp", "ssrd", "strd", "u10", "v10"):
                source.createVariable(
                    name, "f", ("time", "member", "latitude", "longitude")
                )[:] = value
    points = pd.DataFrame(dict(index=[1, 2], lat=[0.5, 5], lon=[0.5, 0.5]))
    result = sources.weather(points, pd.DatetimeIndex(["2019-12-31 21:00"], tz="UTC"))
    for name, value in result.items():
        assert value[0, 0] == (20 if name in ("tp", "ssrd", "strd") else 10)
        assert np.isnan(value[0, 1])
    with pytest.raises(ValueError, match="ERA time axis"):
        sources.weather(points, pd.DatetimeIndex(["2020-02-29"], tz="UTC"))


def test_short_vegetation_gaps():
    dates = pd.to_datetime(["2020-12-18", "2021-01-01", "2021-01-17", "2021-02-02"], utc=True)
    values = np.array([[0], [np.nan], [np.nan], [1]])
    filled, flags = short_gaps_matrix(values, dates)
    np.testing.assert_allclose(filled[:, 0], [0, 14 / 46, 30 / 46, 1])
    assert flags[:, 0].tolist() == [0, 1, 1, 0]
    separate, _ = short_gaps_matrix(values, dates, ["Terra", "Aqua", "Aqua", "Aqua"])
    assert np.isnan(separate[1:3]).all()


def test_timezone_exclusions_and_missing_table_ids(tmp_path):
    from types import SimpleNamespace
    from pecan_downscaling.domain import timezones

    points = pd.DataFrame(dict(index=[10, 11], lat=[45.0, 45.0], lon=[-90.0, -91.0]))
    table = points.copy()
    table["tz"] = None
    path = tmp_path / "timezones.parquet"
    table.to_parquet(path)
    config = SimpleNamespace(sources=dict(timezones=path))
    assert pd.isna(timezones(config, points)).all()
    table.iloc[:1].to_parquet(path)
    with pytest.raises(ValueError, match="Missing timezone table rows"):
        timezones(config, points)
