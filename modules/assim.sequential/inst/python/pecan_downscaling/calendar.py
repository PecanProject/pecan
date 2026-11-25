"""Explicit nominal periods and the four preceding predictor windows."""

import pandas as pd
import numpy as np


def read_periods(path):
    frame = pd.read_csv(path)
    required = {"period_id", "period_key", "start", "stop"}
    if not required <= set(frame):
        raise ValueError(f"Calendar requires {sorted(required)}")
    if frame.period_id.isna().any() or frame.period_id.duplicated().any():
        raise ValueError("Duplicate/missing period IDs")
    if not np.equal(frame.period_id, np.floor(frame.period_id)).all():
        raise ValueError("Period IDs must be integers")
    frame["start"] = pd.to_datetime(frame.start, utc=True)
    frame["stop"] = pd.to_datetime(frame.stop, utc=True)
    steps = (frame.stop - frame.start).dt.total_seconds() / 10800
    if not ((steps > 0) & (steps == np.floor(steps))).all():
        raise ValueError("Periods require positive three-hour windows")
    if frame[["start", "stop", "period_key"]].isna().any().any():
        raise ValueError("Missing calendar fields")
    frame = frame.sort_values("period_id").reset_index(drop=True)
    if (frame.start.iloc[1:].to_numpy() < frame.stop.iloc[:-1].to_numpy()).any():
        raise ValueError("Overlapping periods")
    return frame


def selection(calendar, period_ids):
    lookup = set(calendar.period_id)
    ids = sorted({i - lag for i in period_ids for lag in range(5)})
    missing = sorted(set(ids) - lookup)
    if missing:
        raise ValueError(f"Missing periods or lag support: {missing}")
    return (
        calendar[calendar.period_id.isin(ids)],
        calendar[calendar.period_id.isin(period_ids)],
    )


def vi_calendar(start, stop):
    dates = []
    for year in range(pd.Timestamp(start).year, pd.Timestamp(stop).year + 1):
        dates.extend(
            pd.date_range(
                pd.Timestamp(f"{year}-01-09", tz="UTC"),
                pd.Timestamp(f"{year}-12-31", tz="UTC"),
                freq="16D",
            )
        )
    return pd.DatetimeIndex(dates)
