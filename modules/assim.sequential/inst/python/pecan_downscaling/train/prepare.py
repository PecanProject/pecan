"""Build the proxy panel and native training shards from PEcAn station products."""

from contextlib import ExitStack
from pathlib import Path
from tempfile import TemporaryDirectory
from types import SimpleNamespace

import h5py
import numpy as np
import pandas as pd
import yaml

from pecan_downscaling.artifacts import digest, identity, read, sha, write
from pecan_downscaling.calendar import read_periods, selection
from pecan_downscaling.contracts import (
    KGC,
    NATIVE,
    NUMERIC,
    SOIL,
    add_previous,
    ppfd,
    predictors,
    rootzone,
    validate_lc,
)
from pecan_downscaling.predict.inputs import SpatialSources
from pecan_downscaling.targets.periods import MEMBERS, ROUTES, reduce_members
from pecan_downscaling.targets.stations import TargetInputs, aqua_values, check_weather
from pecan_downscaling.train.data import publish
from pecan_downscaling.train.splits import declare_nested


class Inputs:
    def __init__(self, config):
        self.config = Path(config).resolve(strict=True)
        cfg = yaml.safe_load(self.config.read_text())
        keys = {"stations", "targets", "folds", "annual_covariates", "calendar"}
        if not isinstance(cfg, dict) or set(cfg) - {"resources"} != keys:
            raise ValueError(
                f"Specify training inputs {sorted(keys)} and optional resources"
            )
        self.resources = cfg.get("resources", {})
        for key in keys:
            setattr(self, key, (self.config.parent / cfg[key]).resolve(strict=True))
        self.station = TargetInputs(self.stations, need_fluxes=False)
        self.periods = self.station.periods
        self.needed, wanted = selection(
            read_periods(self.calendar), self.periods.period_id
        )
        fields = ("period_id", "period_key", "start", "stop")
        if len(wanted) != len(self.periods) or any(
            not (wanted[k].reset_index(drop=True) == self.periods[k]).all()
            for k in fields
        ):
            raise ValueError(
                "Station and predictor calendars disagree on target windows"
            )
        if "hydrological_year" in self.periods:
            for station, predictor in zip(
                self.periods.itertuples(), wanted.itertuples()
            ):
                if proxy_year(station) != proxy_year(predictor):
                    raise ValueError(
                        f"Hydrological year mismatch at period {station.period_id}"
                    )
        if (self.periods.expected_3hour_steps > 128).any():
            raise ValueError(
                "Residual model supports up to 128 three-hour steps per period"
            )
        self.target_ids = set(self.periods.period_id)
        self.groups = self.station.sites.groupby(
            (self.station.sites["index"] - 1) // 500 + 1
        )

    def source_id(self):
        # Bind the specific station partitions and raw environmental files used
        # to prepare these arrays. Resume also checks the published array hashes.
        paths = {self.config, self.stations, self.calendar, self.folds}
        station = self.station.paths
        paths.update(
            station[k] for k in ("sites", "timezones", "periods", "kgc_legend")
        )
        registry = yaml.safe_load(self.stations.read_text())["sources"].get(
            "gldas_registry"
        )
        if registry:
            paths.add(self.stations.parent / registry)
        paths.update(station["aqua_points"].glob("*MYD13Q1*results.csv"))
        for t in ("nee", "le"):
            paths.add(self.targets / "targets" / f"{t}.h5")
        for p in self.needed.itertuples():
            times = period_times(p)
            if p.period_id in self.target_ids:
                paths.add(self.annual_covariates / f"covariates_{proxy_year(p)}.tiff")
                for year in times.year.unique():
                    for chunk in self.groups.indices:
                        name = partition_name(year, chunk, p.period_id)
                        paths.add(self.targets / "features" / f"{name}.parquet")
                        paths.update(
                            self.targets / "members" / name / f"{t}.parquet"
                            for t in ("NEE", "LE")
                        )
            else:
                for year in range(p.start.year, p.stop.year + 1):
                    paths.add(station["era"] / f"ERA5_{year}.nc")
                for t in times:
                    if t in self.station.repairs:
                        paths.add(self.station.repairs[t][0])
                    else:
                        paths.add(
                            station["gldas"]
                            / f"GLDAS_NOAH025_3H.A{t:%Y%m%d.%H}00.021.nc4.SUB.nc4"
                        )
        package = Path(__file__).resolve().parents[1]
        components = (
            "artifacts.py",
            "calendar.py",
            "contracts.py",
            "models/bundle.py",
            "targets/stations.py",
            "targets/periods.py",
            "predict/inputs.py",
            "train/splits.py",
            "train/data.py",
            "train/prepare.py",
        )
        return digest(
            dict(
                files=[identity(path) for path in sorted(paths)],
                code={k: sha(package / k) for k in components},
            )
        )


def period_times(p):
    return pd.date_range(p.start, p.stop, freq="3h", inclusive="left")


def proxy_year(p):
    year = getattr(p, "hydrological_year", None)
    return (
        int(year)
        if pd.notna(year)
        else p.start.year + int(p.start.strftime("%m-%d") >= "07-16")
    )


def partition_name(year, chunk, period):
    return f"{year}_{chunk:02d}_p{period}"


def read_frame(path, keys):
    frame = pd.read_parquet(path).set_index(["index", "utc"])
    if (
        not frame.index.is_unique
        or len(frame) != len(keys)
        or not keys.isin(frame.index).all()
    ):
        raise ValueError(f"Duplicate, missing or unexpected station/time keys: {path}")
    return frame.loc[keys]


def environment(weather, layers):
    tc, dc = weather["t2m"] - 273.15, weather["d2m"] - 273.15
    es = 0.6108 * np.exp(17.27 * tc / (tc + 237.3))
    ea = 0.6108 * np.exp(17.27 * dc / (dc + 237.3))
    return dict(
        temp_c=tc,
        precipitation_m=weather["tp"],
        shortwave_w_m2=weather["ssrd"] / 10800,
        vpd_kpa=np.maximum(es - ea, 0),
        rh_fraction=np.clip(ea / es, 0, 1),
        ppfd_umol_m2_s=ppfd(weather["ssrd"]),
        wind_speed_m_s=np.hypot(weather["u10"], weather["v10"]),
        gldas_rootzone_vwc=rootzone(layers),
    )


def history_panel(inputs, source, points, times):
    sums, counts = {}, {}
    for start in range(0, len(times), 8):
        block = times[start : start + 8]
        soil = [inputs.station.soil(points, t) for t in block]
        layers = np.stack([np.column_stack([s[k] for k in SOIL]) for s in soil])
        for name, values in environment(source.weather(points, block), layers).items():
            if name not in sums:
                sums[name] = np.zeros(len(points))
                counts[name] = np.zeros(len(points), int)
            for row in values:  # Original time-ordered float64 sums.
                good = np.isfinite(row)
                sums[name] += np.where(good, row, 0)
                counts[name] += good
    result = {
        k: np.divide(v, counts[k], out=np.full_like(v, np.nan), where=counts[k] > 0)
        for k, v in sums.items()
    }
    result["precipitation_m"] = np.where(
        counts["precipitation_m"] == len(times), sums["precipitation_m"], np.nan
    )
    return pd.DataFrame(result).assign(index=points["index"].to_numpy())


def step_targets(frame):
    values = frame[MEMBERS].to_numpy(dtype=float)
    routes = frame[ROUTES].to_numpy()
    # The mean deliberately requires all 25 members for this flux.
    return values.mean(axis=1), np.stack(
        [(routes == j).mean(axis=1) for j in (1, 2, 3)], axis=1
    )


def target_frames(inputs, points, chunk, p, times, files):
    ids = points["index"]
    parts, grids, members = [], [], [[], []]
    for year in times.year.unique():
        name = partition_name(year, chunk, p.period_id)
        keys = pd.MultiIndex.from_product(
            [ids, times[times.year == year]], names=["index", "utc"]
        )
        grids.append(read_frame(inputs.targets / "features" / f"{name}.parquet", keys))
        parts.append(dict(year=year, name=name))
        for n, t in enumerate(("NEE", "LE")):
            members[n].append(
                read_frame(inputs.targets / "members" / name / f"{t}.parquet", keys)
            )
    keys = pd.MultiIndex.from_product([ids, times], names=["index", "utc"])
    grid = pd.concat(grids).loc[keys]
    for coord in ("lat", "lon"):
        match = grid[coord].to_numpy() == np.repeat(
            points[coord].to_numpy(), len(times)
        )
        if not match.all():
            raise ValueError(
                f"Station {coord} mismatch at {grid.index[np.flatnonzero(~match)[0]]}"
            )
    by_site = pd.DataFrame(environment(grid, grid[SOIL]), index=keys).groupby(
        level="index", sort=False
    )
    panel = by_site.mean()
    panel["precipitation_m"] = by_site.precipitation_m.sum(min_count=len(times))
    raw, y, route = [], [], []
    pos = inputs.periods.index[inputs.periods.period_id == p.period_id][0]
    cols = points.index.to_numpy()
    for t, tables in zip(("NEE", "LE"), members):
        f = pd.concat(tables).loc[keys]
        r = f.raw.to_numpy().reshape(len(ids), len(times))
        raw.append(r)
        target, fractions = step_targets(f)
        y.append(target)
        route.append(fractions)
        reduced = reduce_members(
            inputs.targets / "members",
            parts,
            t,
            ids,
            times,
            loaded=dict(zip(times.year.unique(), tables)),
        )
        for key, expected in zip(("values", "support_counts", "route_codes"), reduced):
            got = files[t][key][pos, :, cols]
            if not np.array_equal(got, expected.astype(got.dtype), equal_nan=True):
                raise ValueError(
                    f"{t} {key} mismatch at period {p.period_id}, station group {chunk}"
                )
    return (
        panel.reset_index(),
        grid,
        np.stack(raw, axis=-1),
        np.column_stack(y),
        np.stack(route, axis=1),
    )


def write_shard(root, year, chunk, batches):
    name = f"{year}_{chunk:02d}"
    # Keep one year/site group in memory, in chronological order within site.
    sites = np.concatenate([b["site"] for b in batches])
    steps = np.concatenate([b["step"] for b in batches])
    order = np.lexsort((steps, sites))
    for key in ("row", "step", "x", "y", "route"):
        np.save(
            root / "native" / f"{name}.{key}.npy",
            np.concatenate([b[key] for b in batches])[order],
        )
    write(root / "native" / f"{name}.json", dict(name=name, rows=len(order)))
    return name


def build_site_group(inputs, root, chunk, points, files, src, dates, vi, arrays):
    periods = inputs.periods
    origin = periods.start.iloc[0]
    n = len(inputs.station.sites)
    names = []
    history, batches = {}, []
    active_year = None
    for p in inputs.needed.itertuples():
        times = period_times(p)
        wanted = p.period_id in inputs.target_ids
        if wanted:
            panel, grid, raw, y, route = target_frames(
                inputs, points, chunk, p, times, files
            )
        else:
            panel = history_panel(inputs, src, points, times)
        center = pd.Timestamp(p.period_key, tz="UTC")
        ix = abs(dates - center).argmin()
        if abs(dates[ix] - center) > pd.Timedelta(days=9):
            raise ValueError(f"No representative Aqua composite: {center}")
        for k in ("EVI", "NDVI"):
            panel[k] = vi[k][ix, points.index]
        panel["period_id"], panel["period_key"] = p.period_id, p.period_key
        history[p.period_id] = panel
        for old in list(history):
            if old < p.period_id - 4:
                del history[old]
        if not wanted:
            continue
        panel = add_previous(panel, history)
        static = src.sample(
            inputs.annual_covariates / f"covariates_{proxy_year(p)}.tiff",
            points,
            [1, 2, 3, 8, 9, 10, 11, 15],
        )
        validate_lc(static[:, 0], "annual proxy LC")
        panel[
            [
                "LC_proxy",
                "year_since_disturb",
                "agb",
                "PH",
                "N",
                "SOC",
                "Sand",
                "DEM",
            ]
        ] = static
        panel[["lat", "lon"]] = points[["lat", "lon"]].to_numpy()
        numeric, lc, eligible = predictors(panel)
        if not np.isfinite(raw[eligible]).all():
            raise ValueError(
                f"Incomplete raw flux at period {p.period_id}, sites {points.loc[eligible & ~np.isfinite(raw).all(axis=(1,2)), 'index'].tolist()}"
            )
        finite = np.isfinite(raw)
        count = finite.sum(axis=1)
        raw_mean = np.divide(
            np.where(finite, raw, 0).sum(axis=1),
            count,
            out=np.full(count.shape, np.nan),
            where=count > 0,
        )
        period_pos = periods.index[periods.period_id == p.period_id][0]
        positions = period_pos * n + points.index.to_numpy()
        for key, val in dict(
            numeric=numeric,
            lc=np.nan_to_num(lc),
            complete=eligible,
            target_mean=raw_mean,
        ).items():
            arrays[key][positions] = val
        kg = grid.KGC.map(dict(zip(KGC, range(len(KGC))))).to_numpy(dtype=float)
        if (grid.KGC.notna() & ~np.isfinite(kg)).any():
            raise ValueError("Unknown native KGC")
        native_lc = grid.LC.to_numpy()
        good = (
            np.repeat(eligible, len(times))
            & np.isfinite(kg)
            & np.isfinite(native_lc)
            & np.isfinite(y).all(axis=1)
        )
        stamp = grid.index.get_level_values("utc")
        native = dict(
            row=np.repeat(positions, len(times)).astype("i4"),
            step=((stamp - origin) / pd.Timedelta(hours=3)).to_numpy(dtype="i4"),
            x=np.column_stack([grid[NATIVE], kg, native_lc]).astype("f4"),
            y=y.astype("f4"),
            route=route.astype("f4"),
            site=grid.index.get_level_values("index").to_numpy(),
        )
        for year in times.year.unique():
            if batches and year != active_year:
                names.append(write_shard(root, active_year, chunk, batches))
                batches = []
            active_year = year
            use = good & (stamp.year == year)
            batches.append({k: v[use] for k, v in native.items()})
    names.append(write_shard(root, active_year, chunk, batches))
    return names


def build(inputs, root, source_id):
    sites, periods = inputs.station.sites, inputs.periods
    inv = pd.read_csv(inputs.folds).sort_values("site_position").reset_index(drop=True)
    if inv.site_position.duplicated().any() or inv.site_position.isna().any():
        raise ValueError("Invalid site_position in folds")
    inv = declare_nested(inv)
    split_cols = [
        "index",
        "site_position",
        "LC",
        "is_tower",
        "fold",
        *[k for k in inv if k.startswith(("inner_", "nested_"))],
    ]
    folds = sites.merge(inv[split_cols], on="index", how="left", validate="one_to_one")
    if folds.fold.isna().any():
        raise ValueError(
            f"Missing fold sites: {folds.loc[folds.fold.isna(),'index'].tolist()}"
        )
    folds.to_csv(root / "folds.csv", index=False)
    periods.assign(start_utc=periods.start).drop(columns=["start", "stop"]).to_csv(
        root / "periods.csv", index=False
    )
    (root / "panel").mkdir()
    (root / "native").mkdir()
    n = len(sites)
    rows = n * len(periods)
    shapes = dict(
        numeric=(rows, len(NUMERIC)),
        lc=(rows,),
        complete=(rows,),
        target_mean=(rows, 2),
    )
    arrays = {
        k: np.lib.format.open_memmap(
            root / "panel" / f"{k}.npy",
            mode="w+",
            dtype=dict(numeric="f4", lc="i8", complete="?", target_mean="f4")[k],
            shape=v,
        )
        for k, v in shapes.items()
    }
    np.save(root / "panel/site_ids.npy", sites["index"].to_numpy())
    np.save(
        root / "panel/years.npy",
        np.repeat(pd.to_datetime(periods.period_key).dt.year.to_numpy(), n),
    )
    dates, vi = aqua_values(
        SimpleNamespace(paths=inputs.station.paths, sites=sites, periods=inputs.needed)
    )
    origin = periods.start.iloc[0]
    names = []
    with ExitStack() as stack:
        src = SpatialSources(inputs.station.paths)
        stack.callback(src.close)
        files = {}
        for t in ("NEE", "LE"):
            h = stack.enter_context(
                h5py.File(inputs.targets / "targets" / f"{t.lower()}.h5")
            )
            for key, labels in dict(
                period_keys=periods.period_key,
                site_ids=sites["index"],
                ensemble_ids=MEMBERS,
            ).items():
                if list(h[key].asstr()[:]) != list(map(str, labels)):
                    raise ValueError(f"{t} HDF5 axis mismatch: {key}")
            if (
                h.attrs["target"] != t
                or not np.array_equal(
                    h["expected_available_steps"][:], periods.expected_3hour_steps
                )
                or not np.array_equal(
                    h["full_window_fraction"][:], periods.full_window_fraction
                )
            ):
                raise ValueError(f"{t} HDF5 window or target mismatch")
            files[t] = h
        for year in sorted(
            {
                y
                for p in inputs.needed.itertuples()
                if p.period_id not in inputs.target_ids
                for y in range(p.start.year, p.stop.year + 1)
            }
        ):
            check_weather(inputs.station.paths["era"] / f"ERA5_{year}.nc")
        for chunk, points in inputs.groups:
            names.extend(
                build_site_group(
                    inputs, root, chunk, points, files, src, dates, vi, arrays
                )
            )
    for array in arrays.values():
        array.flush()
    if inputs.source_id() != source_id:
        raise ValueError("Training inputs changed during preparation")
    publish(root, sorted(names), dict(station_inputs=source_id), str(origin))


def prepare(inputs, destination, resume=False):
    token = inputs.source_id()
    if destination.exists():
        if not resume or read(destination / "manifest.json")["provenance"] != dict(
            station_inputs=token
        ):
            raise ValueError(
                "Existing prepared arrays require matching inputs and resume"
            )
        return
    with TemporaryDirectory(
        prefix=".flux-arrays-", dir=destination.parent.parent
    ) as scratch:
        root = Path(scratch) / "arrays"
        root.mkdir()
        build(inputs, root, token)
        root.rename(destination)
