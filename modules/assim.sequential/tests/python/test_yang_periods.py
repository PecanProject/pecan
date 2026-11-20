from types import SimpleNamespace

import h5py
import numpy as np
import pandas as pd
import pytest

from pecan_downscaling.targets.periods import MEMBERS, ROUTES, write_periods


def test_partial_period_spanning_new_year(tmp_path):
    times = pd.date_range("2012-12-31 18:00", periods=4, freq="3h", tz="UTC")
    periods = pd.DataFrame(
        [
            dict(
                period_id=9,
                period_key="2013-01-01",
                start=times[0],
                stop=times[-1] + pd.Timedelta(hours=3),
                expected_3hour_steps=4,
                full_window_fraction=4 / 128,
            )
        ]
    )
    sites = pd.DataFrame({"index": [702, 7, 701]})
    inputs = SimpleNamespace(sites=sites, periods=periods)
    parts = []
    for chunk, group in sites.groupby((sites["index"] - 1) // 500 + 1):
        for year in (2012, 2013):
            name = f"{year}_{chunk}"
            part = tmp_path / "members" / name
            part.mkdir(parents=True)
            parts.append(dict(name=name, year=year, period=9, chunk=chunk))
            keys = pd.MultiIndex.from_product(
                [group["index"], times[times.year == year]], names=["index", "utc"]
            ).to_frame(index=False)
            pos = times.get_indexer(keys.utc)
            route = np.broadcast_to(
                np.array([1, 2, 0, 3])[pos, None], (len(keys), 25)
            ).copy()
            route[:, 1] = 0
            for target, offset in (("NEE", 0), ("LE", 5)):
                values = np.where(
                    route,
                    keys["index"].to_numpy()[:, None] + pos[:, None] + offset,
                    np.nan,
                )
                frame = pd.concat(
                    [
                        keys,
                        pd.DataFrame(values, columns=MEMBERS),
                        pd.DataFrame(route, columns=ROUTES),
                    ],
                    axis=1,
                )
                frame.iloc[::-1].to_parquet(part / f"{target}.parquet", index=False)
    write_periods(inputs, parts, tmp_path)
    for target, offset in (("nee", 0), ("le", 5)):
        with h5py.File(tmp_path / f"targets/{target}.h5") as h:
            assert list(h["site_ids"].asstr()[:]) == ["702", "7", "701"]
            assert list(h["period_keys"].asstr()[:]) == ["2013-01-01"]
            assert list(h["ensemble_ids"].asstr()[:]) == MEMBERS
            assert h["values"].shape == (1, 25, 3)
            assert h["expected_available_steps"][0] == 4
            assert h["full_window_fraction"][0] == 4 / 128
            np.testing.assert_allclose(
                h["values"][0, 0], sites["index"] + 4 / 3 + offset
            )
            assert np.isnan(h["values"][0, 1]).all()
            np.testing.assert_array_equal(
                h["support_counts"][0, :2], [[3] * 3, [0] * 3]
            )
            np.testing.assert_array_equal(h["route_codes"][0, :2], [[1] * 3, [0] * 3])
    path = part / "NEE.parquet"
    frame = pd.read_parquet(path)
    frame.iloc[0, frame.columns.get_loc("route01")] = 1
    frame.iloc[0, frame.columns.get_loc("ens01")] = np.nan
    frame.to_parquet(path)
    from pecan_downscaling.targets.periods import reduce_members

    with pytest.raises(ValueError, match="Inconsistent member"):
        reduce_members(
            tmp_path / "members",
            [p for p in parts if p["chunk"] == chunk],
            "NEE",
            group["index"],
            times,
        )
