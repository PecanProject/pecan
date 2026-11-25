"""Three-hour station covariates from the native station data products."""

from pathlib import Path
from zoneinfo import ZoneInfo

import h5py
import numpy as np
import pandas as pd
import yaml
from scipy.io import netcdf_file
from scipy.spatial import cKDTree

from pecan_downscaling.artifacts import read, sha
from pecan_downscaling.calendar import vi_calendar
from pecan_downscaling.contracts import SOIL, ppfd
from pecan_downscaling.predict.inputs import SpatialSources, nearest, short_gaps_matrix


class TargetInputs:
    def __init__(self, config):
        config = Path(config).resolve()
        sources = yaml.safe_load(config.read_text())["sources"]
        keys = (
            "sites",
            "timezones",
            "periods",
            "matched_towers",
            "tower",
            "site_means",
            "era",
            "gldas",
            "aqua_points",
            "native_covariates",
            "native_lc",
            "kgc",
            "kgc_legend",
        )
        self.paths = {
            k: (config.parent / sources[k]).resolve(strict=True) for k in keys
        }
        sites = pd.read_csv(self.paths["sites"]).sort_values("site_col")
        for name in ("index", "site_col"):
            v = sites[name]
            if (
                sites.empty
                or v.isna().any()
                or v.duplicated().any()
                or not ((v > 0) & (v == np.floor(v))).all()
            ):
                raise ValueError(f"Invalid or duplicate station {name}")
            sites[name] = v.astype("int64")
        if not (sites.lat.between(-90, 90) & sites.lon.between(-180, 180)).all():
            raise ValueError("Invalid station coordinates")
        if sites.duplicated(["lat", "lon"]).any():
            raise ValueError("Ambiguous coordinates for Aqua point matching")
        self.sites = sites.merge(
            pd.read_csv(self.paths["timezones"])[["index", "tz"]],
            how="left",
            on="index",
            validate="one_to_one",
        )
        if self.sites.tz.isna().any():
            raise ValueError("Missing station timezone")
        self.periods = (
            pd.read_csv(self.paths["periods"])
            .sort_values("period_id")
            .reset_index(drop=True)
        )
        p = self.periods
        p["start"] = pd.to_datetime(p.start_date, utc=True)
        p["stop"] = pd.to_datetime(p.end_date, utc=True) + pd.Timedelta(days=1)
        steps = (p.stop - p.start) / pd.Timedelta(hours=3)
        full = p.full_window_expected_steps
        if (
            p.empty
            or p[["period_id", "period_key", "start", "stop"]].isna().any().any()
            or p.period_id.duplicated().any()
            or p.period_key.duplicated().any()
            or not (p.period_id == np.floor(p.period_id)).all()
            or not (
                (p.start == p.start.dt.normalize()) & (p.stop == p.stop.dt.normalize())
            ).all()
            or (p.start.iloc[1:].to_numpy() < p.stop.iloc[:-1].to_numpy()).any()
            or not (
                (steps > 0)
                & (steps <= 65535)
                & (steps == p.expected_3hour_steps)
                & (np.isfinite(full))
                & (full >= steps)
            ).all()
            or not np.allclose(steps / full, p.full_window_fraction, rtol=0, atol=1e-8)
        ):
            raise ValueError("Invalid, overlapping or inconsistent site periods")
        p["period_id"] = p.period_id.astype("int64")
        self.repairs = {}
        if sources.get("gldas_registry"):
            reg = (config.parent / sources["gldas_registry"]).resolve(strict=True)
            registry = read(reg)
            if registry["schema"] != "gldas-replacements-1":
                raise ValueError("Unknown GLDAS replacement registry")
            for row in registry["replacements"]:
                t = pd.Timestamp(row["utc"])
                if t.tz is None or t in self.repairs:
                    raise ValueError(f"Ambiguous GLDAS replacement time: {t}")
                self.repairs[t] = (reg.parent / row["replacement_path"], row["sha256"])

    def soil(self, points, time):
        if time in self.repairs:
            path, expected = self.repairs[time]
            if sha(path) != expected:
                raise ValueError(f"Changed GLDAS replacement: {path}")
        else:
            path = (
                self.paths["gldas"]
                / f"GLDAS_NOAH025_3H.A{time:%Y%m%d.%H}00.021.nc4.SUB.nc4"
            )
        with h5py.File(path) as src:
            clock = src["time"]
            units = clock.attrs["units"].decode()
            if (
                not units.startswith("minutes since ")
                or len(clock) != 1
                or (
                    pd.to_datetime(units.removeprefix("minutes since "), utc=True)
                    + pd.Timedelta(minutes=float(clock[0]))
                    != time
                )
            ):
                raise ValueError(f"GLDAS time mismatch: {path}")
            lat, lon = src["lat"][:], src["lon"][:]
            rr, cc = nearest(lat, points.lat), nearest(lon, points.lon)
            inside = (
                points.lat.between(lat.min() - 0.125, lat.max() + 0.125)
                & points.lon.between(lon.min() - 0.125, lon.max() + 0.125)
            ).to_numpy()
            result = {}
            for key, depth in zip(SOIL, ("0_10", "10_40", "40_100", "100_200")):
                field = src[f"SoilMoi{depth}cm_inst"]
                if field.attrs["units"].decode() != "kg m-2":
                    raise ValueError(f"Unexpected soil units: {path}")
                block = field[0, rr.min() : rr.max() + 1, cc.min() : cc.max() + 1]
                v = block[rr - rr.min(), cc - cc.min()].astype(float)
                v[(v == np.asarray(field.attrs["_FillValue"]).item()) | ~inside] = (
                    np.nan
                )
                result[key] = v
            return dict(
                **result,
                soil_source_lat=lat[rr],
                soil_source_lon=lon[cc],
                soil_valid_time=time,
            )


def aqua_values(inputs):
    sites, periods = inputs.sites, inputs.periods
    dates = vi_calendar(
        periods.start.min() - pd.Timedelta(days=64),
        periods.stop.max() + pd.Timedelta(days=64),
    )
    prefix = "MYD13Q1_061__250m_16_days_"
    columns = {
        "Latitude": "lat",
        "Longitude": "lon",
        "Date": "date",
        prefix + "NDVI": "NDVI",
        prefix + "EVI": "EVI",
    }
    tree = cKDTree(sites[["lat", "lon"]])
    parts = []
    for path in sorted(inputs.paths["aqua_points"].glob("*MYD13Q1*results.csv")):
        for frame in pd.read_csv(path, usecols=list(columns), chunksize=150000):
            frame = frame.rename(columns=columns)
            coords = frame[["lat", "lon"]].drop_duplicates()
            distance, ix = tree.query(coords)
            coords = coords.assign(index=sites["index"].to_numpy()[ix])[
                distance <= 1e-8
            ]
            frame = frame.merge(coords, on=["lat", "lon"], validate="many_to_one")
            if not frame.empty:
                frame["date"] = pd.to_datetime(frame.date, utc=True)
                parts.append(frame[["index", "date", "NDVI", "EVI"]])
    if not parts:
        raise ValueError("No matching Aqua station exports")
    data = pd.concat(parts, ignore_index=True)
    if (
        set(data["index"]) != set(sites["index"])
        or data.duplicated(["index", "date"]).any()
    ):
        raise ValueError("Missing or duplicate Aqua station records")
    values = {}
    for key in ("NDVI", "EVI"):
        v = pd.to_numeric(data[key], errors="coerce")
        data[key] = v.where(v.between(-0.2, 1))
        table = data.pivot(index="date", columns="index", values=key).reindex(
            index=dates, columns=sites["index"]
        )
        values[key], values[key + "_filled"] = short_gaps_matrix(
            table.to_numpy(), dates
        )
    return dates, values


def station_features(inputs, source, aqua, points, times):
    static = source.native_static(points, times[0].year)
    dates, vi = aqua
    chunks = []
    for offset in range(0, len(times), 8):
        block = times[offset : offset + 8]
        weather = source.weather(points, block)
        for i, t in enumerate(block):
            frame = points[["index", "lon", "lat"]].copy()
            phase = np.empty(len(points))
            for zone in points.tz.unique():
                local = t.tz_convert(ZoneInfo(zone))
                phase[(points.tz == zone).to_numpy()] = (
                    local.dayofyear + (local.hour + local.minute / 60) / 24
                ) / (366 if local.is_leap_year else 365)
            pos = abs(dates - t.normalize()).argmin()
            if abs(dates[pos] - t.normalize()) > pd.Timedelta(days=9):
                raise ValueError(f"No nominal Aqua composite within nine days: {t}")
            frame = frame.assign(
                utc=t,
                **{k: v[i] for k, v in weather.items()},
                **inputs.soil(points, t),
                era_accum_end=t + pd.Timedelta(hours=3),
                **static,
                **{k: v[pos, points.index] for k, v in vi.items()},
                vi_date=dates[pos],
                vi_sensor="Aqua",
                sin_doy=np.sin(2 * np.pi * phase),
                cos_doy=np.cos(2 * np.pi * phase),
            )
            tc, dc = frame.t2m - 273.15, frame.d2m - 273.15
            frame["PPFD"] = ppfd(frame.ssrd)
            frame["WindSpeed"] = np.hypot(frame.u10, frame.v10)
            frame["VPD"] = np.maximum(
                0.6108 * np.exp(17.27 * tc / (tc + 237.3))
                - 0.6108 * np.exp(17.27 * dc / (dc + 237.3)),
                0,
            )
            frame["is_day"] = np.where(
                np.isfinite(frame.ssrd), (frame.ssrd > 10).astype(float), np.nan
            )
            chunks.append(
                frame.assign(
                    t2m2=frame.t2m**2,
                    PPFD2=frame.PPFD**2,
                    EVI2=frame.EVI**2,
                    VPD_t2m=frame.VPD * frame.t2m,
                    sincos=frame.sin_doy * frame.cos_doy,
                )
            )
    return pd.concat(chunks, ignore_index=True).sort_values(["index", "utc"])


def check_weather(path):
    units = dict(
        t2m="K",
        sp="Pa",
        d2m="K",
        tp="m",
        ssrd="J m**-2",
        strd="J m**-2",
        u10="m s**-1",
        v10="m s**-1",
    )
    with netcdf_file(path, "r", mmap=True) as source:
        clock = source.variables["time"]
        unit, _, origin = clock.units.decode().partition(" since ")
        if unit != "hours" or pd.to_datetime(origin, utc=True) != pd.Timestamp(
            "1900-01-01", tz="UTC"
        ):
            raise ValueError(f"Unexpected ERA5 time origin: {path}")
        for name, expected in units.items():
            var = source.variables[name]
            if (
                var.units.decode() != expected
                or len(var.dimensions) != 4
                or var.dimensions[0] != "time"
                or var.dimensions[2:] != ("latitude", "longitude")
            ):
                raise ValueError(
                    f"Unexpected ERA5 units or dimensions for {name}: {path}"
                )
        del clock, var


def write_features(inputs, out):
    out.mkdir()
    years = {
        y
        for p in inputs.periods.itertuples()
        for y in range(p.start.year, p.stop.year + 1)
    }
    for year in sorted(years):
        check_weather(inputs.paths["era"] / f"ERA5_{year}.nc")
    aqua = aqua_values(inputs)
    source = SpatialSources(inputs.paths)
    parts = []
    try:
        for chunk, points in inputs.sites.groupby(
            (inputs.sites["index"] - 1) // 500 + 1
        ):
            for period in inputs.periods.itertuples():
                times = pd.date_range(
                    period.start, period.stop, freq="3h", inclusive="left"
                )
                for year in times.year.unique():
                    name = f"{year}_{chunk:02d}_p{period.period_id}"
                    path = out / f"{name}.parquet"
                    station_features(
                        inputs, source, aqua, points, times[times.year == year]
                    ).to_parquet(path, index=False)
                    parts.append(
                        dict(
                            year=int(year),
                            chunk=int(chunk),
                            period=int(period.period_id),
                            name=name,
                            path=path,
                        )
                    )
    finally:
        source.close()
    return parts
