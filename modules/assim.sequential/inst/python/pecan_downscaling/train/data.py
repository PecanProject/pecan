"""Verified prepared arrays; period-major proxy rows and explicit native shards.

Proxy numeric is float32 (period * site, 67), targets are float32 (row, 2).
Native x is float32 (sample, 33): 31 continuous features, KGC ID, LC ID.
Native y contains NEE/LE in the same physical units as the inference contract.
Native step is a three-hour offset from calendar_start_utc, row is a proxy row.
"""

from pathlib import Path
import numpy as np
import pandas as pd
from pecan_downscaling.artifacts import digest, read, sha, write
from pecan_downscaling.contracts import NUMERIC
from pecan_downscaling.models.bundle import prepared_contract
from pecan_downscaling.train import splits

SCHEMA = "pecan-flux-training-data-1"
PANEL_ARRAYS = ("numeric", "lc", "years", "complete", "target_mean", "site_ids")
NATIVE_ARRAYS = ("x", "y", "row", "step", "route")


def required_files(names):
    if not names or len(set(names)) != len(names):
        raise ValueError("Empty or duplicate expected shard inventory")
    if any(
        not isinstance(name, str)
        or not name
        or Path(name).name != name
        or name in (".", "..")
        for name in names
    ):
        raise ValueError("Invalid shard ID")
    return [
        "folds.csv",
        "periods.csv",
        *[f"panel/{key}.npy" for key in PANEL_ARRAYS],
        *[f"native/{name}.{key}.npy" for name in names for key in NATIVE_ARRAYS],
        *[f"native/{name}.json" for name in names],
    ]


def publish(root, names, provenance, calendar_start_utc):
    """Validate a candidate fully before freezing its manifest; never overwrite."""
    root = Path(root).resolve()
    files = {
        name: dict(sha256=sha(root / name), size=(root / name).stat().st_size)
        for name in required_files(names)
    }
    payload = dict(
        schema=SCHEMA,
        contract=prepared_contract(),
        expected_shards=names,
        calendar_start_utc=calendar_start_utc,
        files=files,
        provenance=provenance,
    )
    candidate = dict(id=digest(payload), **payload)
    Dataset(root, manifest=candidate)
    path = root / "manifest.json"
    if path.exists():
        if read(path) != candidate:
            raise ValueError("Prepared release changed; choose a new directory")
    else:
        write(path, candidate)
    return candidate


def period_position(utc, periods):
    time = pd.DatetimeIndex(pd.to_datetime(utc, utc=True)).as_unit("ns").asi8
    starts = (
        pd.DatetimeIndex(pd.to_datetime(periods.start_utc, utc=True)).as_unit("ns").asi8
    )
    stops = (
        pd.DatetimeIndex(
            pd.to_datetime(periods.end_date, utc=True) + pd.Timedelta(days=1)
        )
        .as_unit("ns")
        .asi8
    )
    positions = np.searchsorted(starts, time, side="right") - 1
    valid = (positions >= 0) & (positions < len(periods))
    valid[valid] &= time[valid] < stops[positions[valid]]
    return positions, valid


class Dataset:
    def __init__(self, root, manifest=None):
        self.root = Path(root).resolve()
        self.manifest = (
            read(self.root / "manifest.json") if manifest is None else manifest
        )
        payload = {key: value for key, value in self.manifest.items() if key != "id"}
        if (
            self.manifest.get("schema") != SCHEMA
            or self.manifest.get("contract") != prepared_contract()
            or digest(payload) != self.manifest.get("id")
        ):
            raise ValueError("Incompatible prepared data manifest")
        self.names = self.manifest["expected_shards"]
        if set(required_files(self.names)) != set(self.manifest["files"]):
            raise ValueError("Prepared data inventory mismatch")
        self.verify_files()
        self.folds = pd.read_csv(self.root / "folds.csv")
        self.periods = pd.read_csv(self.root / "periods.csv")
        self.arrays = {
            key: np.load(
                self.root / f"panel/{key}.npy", mmap_mode="r", allow_pickle=False
            )
            for key in PANEL_ARRAYS
        }
        positions = splits.align_ids(
            self.folds["index"], self.arrays["site_ids"], "panel sites"
        )
        if len(positions) != len(self.folds):
            raise ValueError("Fold/panel site inventory mismatch")
        self.folds = self.folds.iloc[positions].reset_index(drop=True)
        splits.validate(self.folds)
        self.n, self.p = len(self.folds), len(self.periods)
        self.rows = self.n * self.p
        self.start = pd.Timestamp(self.manifest["calendar_start_utc"])
        if self.start.tzinfo is None:
            raise ValueError("Calendar origin must include a timezone")
        self.validate_panel()
        self.shards = [read(self.root / f"native/{name}.json") for name in self.names]
        self.validate_native()

    def verify_files(self):
        for name, expected in self.manifest["files"].items():
            path = (self.root / name).resolve()
            if not path.is_relative_to(self.root) or not path.is_file():
                raise ValueError(f"Missing prepared artifact: {name}")
            if (
                path.stat().st_size != expected["size"]
                or sha(path) != expected["sha256"]
            ):
                raise ValueError(f"Prepared artifact changed: {name}")

    def validate_panel(self):
        p = self.periods
        for key in ("period_id", "period_key"):
            splits.align_ids(p[key], p[key], key)
        starts = pd.to_datetime(p.start_utc, utc=True)
        stops = pd.to_datetime(p.end_date, utc=True) + pd.Timedelta(days=1)
        steps = (stops - starts).dt.total_seconds() / 10800
        if (
            not (
                (steps > 0)
                & (steps == np.floor(steps))
                & (steps == p.expected_3hour_steps)
            ).all()
            or (starts.iloc[1:].to_numpy() < stops.iloc[:-1].to_numpy()).any()
        ):
            raise ValueError("Invalid period boundaries or support")
        if starts.iloc[0] < self.start:
            raise ValueError("Period before native calendar origin")
        a = self.arrays
        for key, shape in dict(
            numeric=(self.rows, len(NUMERIC)),
            target_mean=(self.rows, 2),
            lc=(self.rows,),
            years=(self.rows,),
            complete=(self.rows,),
        ).items():
            if a[key].shape != shape:
                raise ValueError(f"Panel shape mismatch: {key}")
        if (
            a["numeric"].dtype != np.float32
            or a["target_mean"].dtype != np.float32
            or a["complete"].dtype != bool
        ):
            raise ValueError("Panel dtype mismatch")
        if not np.isin(a["lc"], np.arange(12)).all() or not np.issubdtype(
            a["lc"].dtype, np.integer
        ):
            raise ValueError("Unknown proxy LC")
        expected_years = np.repeat(pd.to_datetime(p.period_key).dt.year, self.n)
        if not np.array_equal(a["years"], expected_years):
            raise ValueError("Proxy year/period mismatch")
        eligible = a["complete"]
        if (
            not eligible.any()
            or not np.isfinite(a["numeric"][eligible]).all()
            or not np.isfinite(a["target_mean"][eligible]).all()
        ):
            raise ValueError("Invalid eligible proxy rows")
        active = np.any(eligible.reshape(self.p, self.n), axis=0)
        for key in splits.proxy_keys():
            fit, held = splits.sites(self.folds, *key)
            if not (
                np.isin(self.folds.loc[active, "index"], fit["index"]).any()
                and np.isin(self.folds.loc[active, "index"], held["index"]).any()
            ):
                raise ValueError(f"No active rows in split {key}")

    def native(self, shard, key):
        return np.load(
            self.root / f'native/{shard["name"]}.{key}.npy',
            mmap_mode="r",
            allow_pickle=False,
        )

    def times(self, steps):
        return pd.DatetimeIndex(
            self.start + pd.to_timedelta(steps.astype(np.int64) * 3, unit="h")
        )

    def validate_native(self):
        partitions = set()
        for name, shard in zip(self.names, self.shards):
            if (
                shard.get("name") != name
                or type(shard.get("rows")) is not int
                or shard["rows"] < 0
            ):
                raise ValueError(f"Invalid shard metadata: {name}")
            arrays = {key: self.native(shard, key) for key in NATIVE_ARRAYS}
            count = shard["rows"]
            for key, tail in dict(
                x=(33,), y=(2,), row=(), step=(), route=(2, 3)
            ).items():
                if arrays[key].shape != (count, *tail):
                    raise ValueError(f"Shard shape mismatch: {name}.{key}")
                if key in ("row", "step"):
                    if not np.issubdtype(arrays[key].dtype, np.integer):
                        raise ValueError(f"Noninteger shard indices: {name}.{key}")
                elif arrays[key].dtype != np.float32:
                    raise ValueError(f"Shard dtype mismatch: {name}.{key}")
            row, step = arrays["row"], arrays["step"]
            if np.any(row < 0) or np.any(row >= self.rows) or np.any(step < 0):
                raise ValueError(f"Out-of-range shard indices: {name}")
            if not count:
                continue
            position, valid = period_position(self.times(step), self.periods)
            if (
                not valid.all()
                or not np.array_equal(position, row // self.n)
                or not self.arrays["complete"][row].all()
            ):
                raise ValueError(f"Native/proxy time or eligibility mismatch: {name}")
            pairs = np.column_stack((row % self.n, step))
            if len(np.unique(pairs, axis=0)) != count:
                raise ValueError(f"Duplicate native keys: {name}")
            # Shards partition site/year pairs; detect overlap without a global row set.
            owned = set(
                map(
                    tuple,
                    np.unique(
                        np.column_stack((row % self.n, self.times(step).year)), axis=0
                    ),
                )
            )
            if partitions & owned:
                raise ValueError(f"Overlapping native shard partitions: {name}")
            partitions.update(owned)
            x, y, route = arrays["x"], arrays["y"], arrays["route"]
            if (
                np.isinf(x).any()
                or not np.isfinite(y).all()
                or not np.isfinite(route).all()
                or np.any(route < 0)
                or np.any(route > 1)
                or np.any(route.sum(axis=2) > 1.000001)
            ):
                raise ValueError(f"Invalid native values: {name}")
            if (
                not np.isin(x[:, 31], np.arange(23)).all()
                or not np.isin(x[:, 32], np.arange(1, 9)).all()
            ):
                raise ValueError(f"Unknown native category: {name}")
