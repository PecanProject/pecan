import unittest
import numpy as np
import pandas as pd
from pecan_downscaling.contracts import (
    rootzone,
    ppfd,
    unique,
    STATIC,
    DYNAMIC,
    predictors,
)


class ContractTests(unittest.TestCase):
    def test_rootzone_mass_to_volume(self):
        np.testing.assert_allclose(rootzone([[20, 60, 120, 900]]), [0.2])
        self.assertTrue(np.isnan(rootzone([[20, np.nan, 120, 900]])[0]))

    def test_ppfd_physical_conversion(self):
        self.assertAlmostEqual(float(ppfd(10800 * 100)), 205.65)

    def test_duplicate_and_absent_keys_fail(self):
        for f in [
            pd.DataFrame({"index": [1, 1], "utc": [1, 1]}),
            pd.DataFrame({"index": [1, None], "utc": [1, 2]}),
        ]:
            with self.assertRaises(ValueError):
                unique(f, ["index", "utc"])



def test_calendar_lags_and_clipped_support(tmp_path):
    import pytest
    from pecan_downscaling.calendar import read_periods, selection

    frame = pd.DataFrame(
        {
            "period_id": list(range(-3, 2)),
            "period_key": pd.date_range("2019-12-27", periods=5).astype(str),
            "start": pd.date_range("2019-12-27", periods=5),
            "stop": pd.date_range("2019-12-28", periods=5),
        }
    )
    path = tmp_path / "periods.csv"
    frame.to_csv(path, index=False)
    calendar = read_periods(path)
    needed, wanted = selection(calendar, [1])
    assert len(needed) == 5 and len(wanted) == 1
    with pytest.raises(ValueError, match="lag support"):
        selection(calendar, [2])


def test_feature_order_and_missingness():
    frame = pd.DataFrame(
        {
            n: [0.2, 0.3]
            for n in STATIC
            + DYNAMIC
            + [f"{n}_L{k}" for k in range(1, 5) for n in DYNAMIC]
        }
    )
    frame["lat"] = [45, 60]
    frame["lon"] = [-90, -100]
    frame["period_key"] = ["2020-02-29", "2020-07-19"]
    frame["LC_proxy"] = [1, 11]
    values, lc, valid = predictors(frame)
    assert values.shape == (2, 67) and values.dtype == np.float32 and valid.all()
    assert lc.tolist() == [1, 11]
    frame.loc[0, "temp_c"] = np.nan
    assert predictors(frame)[2].tolist() == [False, True]


def test_calendar_golden_year_leap_and_clipped_endpoint():
    from pathlib import Path
    from pecan_downscaling.calendar import read_periods

    frame = read_periods(Path(__file__).parent / "fixtures/calendar_boundaries.csv")
    for row in frame.itertuples():
        times = pd.date_range(row.start, row.stop, freq="3h", inclusive="left")
        assert len(times) == row.expected_steps
        assert times[-1] + pd.Timedelta(hours=3) == row.stop
    leap = frame[frame.period_id == 176].iloc[0]
    assert leap.start < pd.Timestamp("2020-02-29", tz="UTC") < leap.stop
    # Nominal dates remain intact even when the last observed window is clipped.
    assert frame.iloc[-1].period_key == "2024-07-19"
