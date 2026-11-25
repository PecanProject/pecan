"""Windowed meteorology, soil and vegetation for period inference."""

from collections import OrderedDict
import numpy as np, pandas as pd
import warnings, time, json, hashlib
from pathlib import Path
from types import SimpleNamespace
import rasterio
from rasterio.windows import Window
from rasterio.warp import transform
from scipy.io import netcdf_file
from pecan_downscaling.artifacts import identity, read
from pecan_downscaling.contracts import DYNAMIC, rootzone, ppfd
from pecan_downscaling.contracts import validate_lc


def bracket(axis, values):
    a = np.asarray(axis)
    order = np.argsort(a)
    s = a[order]
    v = np.asarray(values)
    hi = np.searchsorted(s, v, side="right").clip(1, len(s) - 1)
    lo = hi - 1
    w = (v - s[lo]) / (s[hi] - s[lo])
    inside = (v >= s[0]) & (v <= s[-1])
    return order[lo], order[hi], w, inside


def point_signature(points):
    return hashlib.sha256(
        points[["index", "lat", "lon"]].to_numpy(np.float64).tobytes()
    ).hexdigest()


class Sources:
    def __init__(self, paths):
        self.paths = paths
        self.handles, self.geometry, self.windows, self.coarse = (
            OrderedDict() for _ in range(4)
        )
        self.window_bytes = self.coarse_bytes = 0
        self.era_files, self.identities = {}, {}
        self.stats = dict(raster_reads=0, raster_hits=0, era_reads=0, era_hits=0)
        legend = pd.read_csv(paths["kgc_legend"])
        self.legend = dict(zip(legend.code, legend.KGC))
        ledger = read(paths["aqua_ledger"])
        excluded = read(Path(__file__).with_name("point_export_gaps.json"))
        self.dates = pd.to_datetime(ledger["dates"], format="A%Y%j", utc=True)
        self.layers = {
            (pd.Timestamp(r["date"]), r["tile"]): r["layers"]
            for r in ledger["sources"]
            if r["tile"] not in excluded.get(r["date"][:10], [])
        }

    def checked(self, path):
        path = str(path)
        now = identity(path)
        if path in self.identities and self.identities[path] != now:
            raise ValueError(f"Changed source: {path}")
        self.identities[path] = now
        return path

    def sample(self, path, points, bands):
        path = self.checked(path)
        if path not in self.handles:
            if len(self.handles) >= 8:
                self.handles.popitem(last=False)[1].close()
            self.handles[path] = rasterio.open(path)
        self.handles.move_to_end(path)
        src = self.handles[path]
        geomkey = (
            point_signature(points),
            str(src.crs),
            tuple(src.transform),
            src.width,
            src.height,
        )
        if geomkey not in self.geometry:
            x, y = transform(
                "EPSG:4326", src.crs, points.lon.tolist(), points.lat.tolist()
            )
            x = np.asarray(x)
            y = np.asarray(y)
            rows, cols = rasterio.transform.rowcol(src.transform, x, y)
            rows = np.asarray(rows)
            cols = np.asarray(cols)
            valid = (
                (src.bounds.left <= x)
                & (x < src.bounds.right)
                & (src.bounds.bottom < y)
                & (y <= src.bounds.top)
                & (rows >= 0)
                & (rows < src.height)
                & (cols >= 0)
                & (cols < src.width)
            )
            self.geometry[geomkey] = (rows, cols, valid, x, y)
            if len(self.geometry) > 12:
                self.geometry.popitem(last=False)
        rows, cols, valid, x, y = self.geometry[geomkey]
        out = np.full((len(points), len(bands)), np.nan)
        if valid.any():
            rr = rows[valid]
            cc = cols[valid]
            r0, r1 = int(rr.min()), int(rr.max()) + 1
            c0, c1 = int(cc.min()), int(cc.max()) + 1
            if (r1 - r0) * (c1 - c0) > max(1000000, len(rr) * 128):
                out[valid] = [
                    np.ma.asarray(v, dtype=float).filled(np.nan)
                    for v in src.sample(
                        zip(x[valid], y[valid]), indexes=bands, masked=True
                    )
                ]
            else:
                wk = (path, tuple(bands), r0, r1, c0, c1)
                block = self.windows.get(wk)
                if block is None:
                    block = src.read(
                        bands, window=Window(c0, r0, c1 - c0, r1 - r0), masked=True
                    )
                    self.stats["raster_reads"] += 1
                    self.windows[wk] = block
                    self.window_bytes += block.nbytes + np.ma.getmaskarray(block).nbytes
                    while self.window_bytes > 64 * 1024**2 and len(self.windows) > 1:
                        _, old = self.windows.popitem(last=False)
                        self.window_bytes -= old.nbytes + np.ma.getmaskarray(old).nbytes
                else:
                    self.stats["raster_hits"] += 1
                    self.windows.move_to_end(wk)
                out[valid] = block[:, rr - r0, cc - c0].astype(float).filled(np.nan).T
        return out

    def native_static(self, points, year):
        names = ("year_since_disturb", "agb", "twi", "PH", "N", "SOC", "Sand")
        a = self.sample(
            self.paths["native_covariates"] / f"covariates_{year}.tiff",
            points,
            [2, 3, 4, 9, 10, 11, 12],
        )
        out = dict(zip(names, a.T))
        out["LC"] = self.sample(self.paths["native_lc"], points, [1])[:, 0]
        validate_lc(out["LC"], "native LC")
        code = self.sample(self.paths["kgc"], points, [1])[:, 0]
        unknown = set(code[np.isfinite(code)]) - set(self.legend)
        if unknown:
            raise ValueError(f"Unknown KGC: {unknown}")
        out["KGC"] = np.array([self.legend.get(v, None) for v in code])
        return out

    def weather(self, points, times):
        result = {}
        sig = point_signature(points)
        for name in ("t2m", "sp", "d2m", "tp", "ssrd", "strd", "u10", "v10"):
            effective = (
                times + pd.Timedelta(hours=3)
                if name in ("tp", "ssrd", "strd")
                else times
            )
            out = np.full((len(times), len(points)), np.nan)
            for year in np.unique(effective.year):
                path = self.paths["era"] / f"ERA5_{year}.nc"
                self.checked(path)
                if int(year) not in self.era_files:
                    if len(self.era_files) >= 2:
                        self.era_files.pop(next(iter(self.era_files))).close()
                    self.era_files[int(year)] = netcdf_file(path, "r", mmap=True)
                f = self.era_files[int(year)]
                chosen = np.flatnonzero(effective.year == year)
                axis = np.asarray(f.variables["time"][:])
                encoded = (
                    effective[chosen] - pd.Timestamp("1900-01-01", tz="UTC")
                ).total_seconds().to_numpy() / 3600
                pos = np.searchsorted(axis, encoded)
                if (
                    np.any(np.diff(axis) <= 0)
                    or np.any(pos >= len(axis))
                    or not np.array_equal(axis[pos], encoded)
                ):
                    raise ValueError("Invalid ERA time axis")
                gk = ("era", sig, int(year))
                geom = self.geometry.get(gk)
                if geom is None:
                    r0, r1, wy, iy = bracket(f.variables["latitude"][:], points.lat)
                    c0, c1, wx, ix = bracket(f.variables["longitude"][:], points.lon)
                    geom = (r0, r1, wy, iy, c0, c1, wx, ix)
                    self.geometry[gk] = geom
                    if len(self.geometry) > 12:
                        self.geometry.popitem(last=False)
                r0, r1, wy, iy, c0, c1, wx, ix = geom
                # Fixed coarse chunks permit reuse by neighboring analysis-grid blocks.
                rlo = int(min(r0.min(), r1.min())) // 16 * 16
                rhi = min(
                    len(f.variables["latitude"][:]),
                    (int(max(r0.max(), r1.max())) // 16 + 1) * 16,
                )
                clo = int(min(c0.min(), c1.min())) // 16 * 16
                chi = min(
                    len(f.variables["longitude"][:]),
                    (int(max(c0.max(), c1.max())) // 16 + 1) * 16,
                )
                ck = (str(path), name, tuple(pos), rlo, rhi, clo, chi)
                z = self.coarse.get(ck)
                if z is None:
                    v = f.variables[name]
                    raw = v.data[pos, :, rlo:rhi, clo:chi].astype(float)
                    raw[raw == getattr(v, "_FillValue", -32767)] = np.nan
                    raw = raw * getattr(v, "scale_factor", 1) + getattr(
                        v, "add_offset", 0
                    )
                    with warnings.catch_warnings():
                        warnings.simplefilter("ignore", RuntimeWarning)
                        z = np.nanmean(raw, axis=1)
                    del raw, v
                    self.coarse[ck] = z
                    self.coarse_bytes += z.nbytes
                    self.stats["era_reads"] += 1
                    while self.coarse_bytes > 32 * 1024**2 and len(self.coarse) > 1:
                        self.coarse_bytes -= self.coarse.popitem(last=False)[1].nbytes
                else:
                    self.coarse.move_to_end(ck)
                    self.stats["era_hits"] += 1
                val = np.zeros((len(chosen), len(points)))
                corners = (
                    (r0, c0, (1 - wy) * (1 - wx)),
                    (r0, c1, (1 - wy) * wx),
                    (r1, c0, wy * (1 - wx)),
                    (r1, c1, wy * wx),
                )
                if np.isfinite(z).all():
                    total = np.zeros(len(points))
                    temporary = np.empty_like(val)
                    flat = z.reshape(len(chosen), -1)
                    for rr, cc, w in corners:
                        weight = np.where(w > 1e-12, w, 0)
                        columns = (rr - rlo) * (chi - clo) + cc - clo
                        np.take(flat, columns, axis=1, out=temporary)
                        np.multiply(temporary, weight, out=temporary)
                        np.add(val, temporary, out=val)
                        total += weight
                    np.divide(val, total[None, :], out=val, where=total[None, :] > 0)
                    val[:, total <= 0] = np.nan
                else:
                    total = np.zeros_like(val)
                    for rr, cc, w in corners:
                        a = z[:, rr - rlo, cc - clo]
                        good = np.isfinite(a) & (w[None, :] > 1e-12)
                        val += np.where(good, a * w, 0)
                        total += np.where(good, w, 0)
                    np.divide(val, total, out=val, where=total > 0)
                    val[total <= 0] = np.nan
                val[:, ~(ix & iy)] = np.nan
                out[chosen] = val
            result[name] = out
        return result

    def close(self):
        for v in self.handles.values():
            v.close()
        for v in self.era_files.values():
            v.close()
        self.handles.clear()
        self.era_files.clear()
        self.windows.clear()
        self.coarse.clear()

    def vegetation(self, p, needed):
        x, y = transform(
            "EPSG:4326",
            "+proj=sinu +R=6371007.181 +nadgrids=@null +wktext",
            p.lon.tolist(),
            p.lat.tolist(),
        )
        h = np.floor((np.array(x) + 20015109.354) / 1111950.5196666666).astype(int)
        v = np.floor((10007554.677 - np.array(y)) / 1111950.5196666666).astype(int)
        tiles = np.array([f"h{a:02d}v{b:02d}" for a, b in zip(h, v)])
        values = {}
        filled = {}
        dates = self.dates[
            (self.dates >= needed.start.min() - pd.Timedelta(days=64))
            & (self.dates <= needed.stop.max() + pd.Timedelta(days=64))
        ]
        for name in ("NDVI", "EVI"):
            raw = np.full((len(dates), len(p)), np.nan)
            for tile in np.unique(tiles):
                ix = np.flatnonzero(tiles == tile)
                for i, date in enumerate(dates):
                    layers = self.layers.get((date, tile))
                    if layers is None:
                        continue
                    layer = next(
                        r for r in layers if r["path"].endswith("_" + name + ".tif")
                    )
                    self.checked(layer["path"])
                    vals = self.sample(layer["path"], p.iloc[ix], [1])[:, 0]
                    vals[(vals < -2000) | (vals > 10000)] = np.nan
                    raw[i, ix] = vals * 0.0001
            values[name], filled[name] = short_gaps_matrix(raw, dates)
        return SimpleNamespace(dates=dates, values=values, filled=filled)


def short_gaps_matrix(values, dates, sources=None, maximum=2):
    a = np.asarray(values, dtype=float).copy()
    t = pd.DatetimeIndex(dates).asi8
    if a.ndim != 2 or len(a) != len(t) or np.any(np.diff(t) <= 0):
        raise ValueError("Invalid VI calendar")
    s = np.repeat("Aqua", len(t)) if sources is None else np.asarray(sources)
    if len(s) != len(t):
        raise ValueError("Invalid VI source sequence")
    flag = np.zeros(a.shape, np.uint8)
    for i in range(1, len(t) - 1):
        start = np.isfinite(a[i - 1]) & ~np.isfinite(a[i])
        for length in range(1, maximum + 1):
            end = i + length
            if end >= len(t) or not np.all(s[i - 1 : end + 1] == s[i - 1]):
                continue
            mask = start & np.isfinite(a[end]) & (~np.isfinite(a[i:end])).all(axis=0)
            if not mask.any():
                continue
            x0 = float(t[i - 1])
            dx = float(t[end]) - x0
            slope = (a[end, mask] - a[i - 1, mask]) / dx
            for k in range(length):
                a[i + k, mask] = slope * (float(t[i + k]) - x0) + a[i - 1, mask]
                flag[i + k, mask] = 1
    return a, flag


def nearest(axis, values):
    order = np.argsort(axis)
    a = np.asarray(axis)[order]
    hi = np.searchsorted(a, values).clip(1, len(a) - 1)
    lo = hi - 1
    dl = np.abs(a[lo] - values)
    dh = np.abs(a[hi] - values)
    return np.where(
        dl < dh,
        order[lo],
        np.where(dh < dl, order[hi], np.minimum(order[lo], order[hi])),
    )


def soil(points, row, src):
    base = src.paths["soil_cache"] / f"gldas_{int(row.period_id):03d}"
    meta = json.loads(base.with_suffix(".json").read_text())
    if meta["artifact"] != identity(base.with_suffix(".npy")):
        raise ValueError("GLDAS cache identity mismatch")
    if (
        pd.Timestamp(meta["start"]) != row.start
        or pd.Timestamp(meta["stop"]) != row.stop
    ):
        raise ValueError("GLDAS time mismatch")
    a = np.load(base.with_suffix(".npy"), mmap_mode="r")
    lat = np.asarray(meta["lat"])
    lon = np.asarray(meta["lon"])
    rr = nearest(lat, points.lat.to_numpy())
    cc = nearest(lon, points.lon.to_numpy())
    out = a[:, rr, cc, :].copy()
    inside = (
        points.lat.between(lat.min() - 0.125, lat.max() + 0.125)
        & points.lon.between(lon.min() - 0.125, lon.max() + 0.125)
    ).to_numpy()
    out[:, ~inside] = np.nan
    return out


def current_panel(points, vi, row, src, timing):
    times = pd.date_range(row.start, row.stop, freq="3h", inclusive="left")
    n = len(points)
    tick = time.perf_counter()
    weather = src.weather(points, times)
    timing["era_read_interpolate"] = time.perf_counter() - tick
    tick = time.perf_counter()
    layers = soil(points, row, src)
    timing["soil_read_sample"] = time.perf_counter() - tick
    sums = np.zeros((10, n))
    counts = np.zeros((10, n), np.int32)
    derive = aggregate = 0.0
    # Per-cell additions have exactly the reference temporal order.
    for start in range(0, len(times), 16):
        tick = time.perf_counter()
        sl = slice(start, start + 16)
        tc = weather["t2m"][sl] - 273.15
        dc = weather["d2m"][sl] - 273.15
        es = 0.6108 * np.exp(17.27 * tc / (tc + 237.3))
        ea = 0.6108 * np.exp(17.27 * dc / (dc + 237.3))
        arrays = {
            0: tc,
            1: weather["tp"][sl],
            2: weather["ssrd"][sl] / 10800,
            3: np.maximum(es - ea, 0),
            4: np.clip(ea / es, 0, 1),
            5: ppfd(weather["ssrd"][sl]),
            6: np.hypot(weather["u10"][sl], weather["v10"][sl]),
            9: rootzone(layers[sl]),
        }
        derive += time.perf_counter() - tick
        tick = time.perf_counter()
        for j, a in arrays.items():
            for row_values in a:
                good = np.isfinite(row_values)
                sums[j] += np.where(good, row_values, 0)
                counts[j] += good
        aggregate += time.perf_counter() - tick
        del arrays, tc, dc, es, ea
    timing["period_derive"] = derive
    timing["ordered_aggregate"] = aggregate
    tick = time.perf_counter()
    mean = np.divide(sums, counts, out=np.full_like(sums, np.nan), where=counts > 0)
    mean[1] = np.where(counts[1] == len(times), sums[1], np.nan)
    panel = pd.DataFrame(mean.T, columns=DYNAMIC)
    panel["index"] = points["index"].to_numpy()
    panel["period_id"] = int(row.period_id)
    panel["period_key"] = row.period_key
    panel["expected_steps"] = len(times)
    for j, name in enumerate(DYNAMIC):
        panel[name + "_support"] = counts[j]
    date = pd.Timestamp(row.period_key, tz="UTC")
    distance = abs(vi.dates - date.normalize())
    center = int(distance.argmin())
    if distance[center] > pd.Timedelta(days=9):
        raise ValueError("VI match exceeds tolerance")
    for name in ("EVI", "NDVI"):
        panel[name] = vi.values[name][center]
        panel[name + "_support"] = np.isfinite(vi.values[name][center]).astype(int)
        panel[name + "_filled"] = vi.filled[name][center]
    panel["vi_date"] = vi.dates[center]
    panel["vi_sensor"] = "Aqua"
    year = (
        int(row.hydrological_year)
        if hasattr(row, "hydrological_year") and pd.notna(row.hydrological_year)
        else row.start.year + int(row.start.strftime("%m-%d") >= "07-16")
    )
    names = ("LC_proxy", "year_since_disturb", "agb", "PH", "N", "SOC", "Sand", "DEM")
    a = src.sample(
        src.paths["annual_covariates"] / f"covariates_{year}.tiff",
        points,
        [1, 2, 3, 8, 9, 10, 11, 15],
    )
    validate_lc(a[:, 0], "annual proxy LC")
    for j, name in enumerate(names):
        panel[name] = a[:, j]
    panel["lat"] = points.lat.to_numpy()
    panel["lon"] = points.lon.to_numpy()
    timing["panel_static"] = time.perf_counter() - tick
    return panel, weather, layers, times


from zoneinfo import ZoneInfo
from pecan_downscaling.contracts import SOIL, NATIVE


def compact_input(
    points,
    good,
    row,
    times,
    weather,
    layers,
    vi,
    statics,
    reps,
    numeric,
    annual,
    kg,
    lc,
    out=None,
):
    selected = np.flatnonzero(good)
    n = len(selected)
    if out is None:
        out = np.empty((n, len(reps)), np.float32, order="F")
    if (
        out.shape != (n, len(reps))
        or out.dtype != np.float32
        or not out.flags.f_contiguous
    ):
        raise ValueError("Invalid direct compact buffer")
    out.fill(np.nan)
    groups = {}
    # Head columns are written directly, without allocating presence/one-hot blocks.
    years = pd.to_datetime(pd.Series(np.repeat(row.period_key, n))).dt.year.to_numpy()
    offsets = [
        4096,
        4096 + numeric.shape[1],
        4096 + numeric.shape[1] + 23,
        4096 + numeric.shape[1] + 31,
        4096 + numeric.shape[1] + 43,
    ]
    if offsets[-1] != 4206:
        raise ValueError("Unexpected frozen feature layout")
    for col, original in enumerate(reps):
        if original < 3968:
            step, var = divmod(int(original), 31)
            if step < len(times):
                groups.setdefault(step, []).append((col, var))
        elif original < 4096:
            out[:, col] = float(original - 3968 < len(times))
        elif original < offsets[1]:
            out[:, col] = numeric[good, original - offsets[0]]
        elif original < offsets[2]:
            out[:, col] = kg[good] == original - offsets[1]
        elif original < offsets[3]:
            out[:, col] = lc[good] - 1 == original - offsets[2]
        elif original < offsets[4]:
            out[:, col] = annual[good] == original - offsets[3]
        elif original < 4219:
            out[:, col] = years - 2012 == original - 4206
        else:
            out[:, col] = len(times) / 128
    zones = points.tz.to_numpy()
    unique = list(pd.unique(zones))
    zone_code = np.array([unique.index(v) for v in zones])[good]
    phase = np.empty((len(times), len(unique)))
    for j, zone in enumerate(unique):
        local = [t.tz_convert(ZoneInfo(zone)) for t in times]
        phase[:, j] = [
            (d.dayofyear + (d.hour + d.minute / 60) / 24)
            / (366 if d.is_leap_year else 365)
            for d in local
        ]
    sine = np.sin(2 * np.pi * phase)
    cosine = np.cos(2 * np.pi * phase)
    matched = [int(abs(vi.dates - t.normalize()).argmin()) for t in times]
    for step, columns in groups.items():
        w = {k: a[step, good] for k, a in weather.items()}
        tc = w["t2m"] - 273.15
        dc = w["d2m"] - 273.15
        vpd = np.maximum(
            0.6108 * np.exp(17.27 * tc / (tc + 237.3))
            - 0.6108 * np.exp(17.27 * dc / (dc + 237.3)),
            0,
        )
        sw = ppfd(w["ssrd"])
        evi = vi.values["EVI"][matched[step], good]
        ndvi = vi.values["NDVI"][matched[step], good]
        fields = dict(
            w,
            lon=points.lon.to_numpy()[good],
            lat=points.lat.to_numpy()[good],
            sin_doy=sine[step, zone_code],
            cos_doy=cosine[step, zone_code],
            NDVI=ndvi,
            EVI=evi,
            PPFD=sw,
            WindSpeed=np.hypot(w["u10"], w["v10"]),
            t2m2=w["t2m"] ** 2,
            PPFD2=sw**2,
            EVI2=evi**2,
            VPD_t2m=vpd * w["t2m"],
            sincos=sine[step, zone_code] * cosine[step, zone_code],
            VPD=vpd,
            is_day=np.where(
                np.isfinite(w["ssrd"]), (w["ssrd"] > 10).astype(float), np.nan
            ),
        )
        for name in ("year_since_disturb", "agb", "twi", "PH", "N", "SOC", "Sand"):
            fields[name] = statics[times[step].year][name][good]
        for j, name in enumerate(SOIL):
            fields[name] = layers[step, good, j]
        for col, var in columns:
            out[:, col] = fields[NATIVE[var]]
    return out
