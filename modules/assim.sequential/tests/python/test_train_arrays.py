from types import SimpleNamespace

import h5py
import numpy as np
import pandas as pd
import pytest

from pecan_downscaling.contracts import SOIL
from pecan_downscaling.targets.periods import MEMBERS, ROUTES, write_periods
from pecan_downscaling.train.prepare import target_frames


def test_join_across_years_and_member_support(tmp_path):
    clock = pd.date_range("2012-12-31 21:00", periods=3, freq="3h", tz="UTC")
    sites = pd.DataFrame({"index": [8, 3], "lat": [1.0, 2.0], "lon": [3.0, 4.0]})
    periods = pd.DataFrame(
        [
            dict(
                period_id=7,
                period_key="2013-01-01",
                start=clock[0],
                stop=clock[-1] + pd.Timedelta(hours=3),
                expected_3hour_steps=3,
                full_window_fraction=3 / 128,
            )
        ]
    )
    p = next(periods.itertuples())
    inputs = SimpleNamespace(
        station=SimpleNamespace(sites=sites),
        sites=sites,
        periods=periods,
        targets=tmp_path,
    )
    (tmp_path / "features").mkdir()
    parts = []
    for yr in clock.year.unique():
        name = f"{yr}_01_p7"
        parts.append(dict(name=name, chunk=1, year=yr, period=7))
        frame = pd.MultiIndex.from_product(
            [sites["index"], clock[clock.year == yr]], names=["index", "utc"]
        ).to_frame(index=False)
        frame = frame.merge(sites, on="index")
        for key in ["t2m", "d2m", "ssrd", "tp", "u10", "v10", *SOIL]:
            frame[key] = 1.0
        frame.iloc[::-1].to_parquet(
            tmp_path / "features" / f"{name}.parquet", index=False
        )
        path = tmp_path / "members" / name
        path.mkdir(parents=True)
        for t in ("NEE", "LE"):
            member = frame[["index", "utc"]].assign(raw=8.0)
            member[MEMBERS] = np.broadcast_to(
                np.arange(25, dtype=float), (len(member), 25)
            )
            member[ROUTES] = 3
            if t == "NEE":
                missing = member.utc == clock[1]
                member.loc[missing, "ens03"] = np.nan
                member.loc[missing, "route03"] = 0
            member.iloc[::-1].to_parquet(path / f"{t}.parquet", index=False)
    write_periods(inputs, parts, tmp_path)
    with h5py.File(tmp_path / "targets/nee.h5", "r+") as nee, h5py.File(
        tmp_path / "targets/le.h5"
    ) as le:
        files = {"NEE": nee, "LE": le}
        _, grid, raw, y, route = target_frames(inputs, sites, 1, p, clock, files)
        assert grid.index.tolist() == [(s, t) for s in (8, 3) for t in clock]
        np.testing.assert_array_equal(raw, np.full((2, 3, 2), 8.0))
        np.testing.assert_array_equal(y[:, 0], [12, np.nan, 12] * 2)
        assert (y[:, 1] == 12).all()
        assert route[1, 0, 2] == 24 / 25
        nee["support_counts"][0, 2, 0] = 3
        with pytest.raises(ValueError, match="support_counts mismatch"):
            target_frames(inputs, sites, 1, p, clock, files)
