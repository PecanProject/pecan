import atexit, time, json
from zoneinfo import ZoneInfo
import numpy as np, pandas as pd
from scipy.io import netcdf_file
from pecan_downscaling.domain import (
    land_mask,
    land_points,
    land_sources,
    point_block,
    timezones,
)
from pecan_downscaling.contracts import add_previous
from pecan_downscaling.predict.inputs import bracket
from pecan_downscaling.contracts import KGC, unique, predictors
from pecan_downscaling.artifacts import identity
from pecan_downscaling.predict.inputs import Sources, nearest, compact_input, soil
from pecan_downscaling.predict.panels import load_panels


def initialize(ring, needed, row, config, token, reps):
    global STATE
    src = Sources(config.sources)
    STATE = ring, needed, row, src, config, token, reps
    atexit.register(src.close)


def coarse(points, times, src):
    fields = []
    geometry = {}
    fallback = {}
    for name in ("t2m", "sp", "d2m", "tp", "ssrd", "strd", "u10", "v10"):
        effective = (
            times + pd.Timedelta(hours=3) if name in ("tp", "ssrd", "strd") else times
        )
        out = (
            np.full((len(times), len(points)), np.nan)
            if name in ("t2m", "d2m", "u10", "v10")
            else None
        )
        for year in np.unique(effective.year):
            path = src.paths["era"] / f"ERA5_{year}.nc"
            src.checked(path)
            if int(year) not in src.era_files:
                src.era_files[int(year)] = netcdf_file(path, "r", mmap=True)
            f = src.era_files[int(year)]
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
                raise ValueError("ERA window mismatch")
            if int(year) not in geometry:
                r0, r1, wy, iy = bracket(f.variables["latitude"][:], points.lat)
                c0, c1, wx, ix = bracket(f.variables["longitude"][:], points.lon)
                geometry[int(year)] = (r0, r1, wy, iy, c0, c1, wx, ix)
            r0, r1, wy, iy, c0, c1, wx, ix = geometry[int(year)]
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
            z = src.coarse.get(ck)
            if z is None:
                import warnings

                v = f.variables[name]
                raw = v.data[pos, :, rlo:rhi, clo:chi].astype(float)
                raw[raw == getattr(v, "_FillValue", -32767)] = np.nan
                raw = raw * getattr(v, "scale_factor", 1) + getattr(v, "add_offset", 0)
                with warnings.catch_warnings():
                    warnings.simplefilter("ignore", RuntimeWarning)
                    z = np.nanmean(raw, axis=1)
                del raw, v
                src.coarse[ck] = z
                src.coarse_bytes += z.nbytes
                while src.coarse_bytes > 32 * 1024**2 and len(src.coarse) > 1:
                    src.coarse_bytes -= src.coarse.popitem(last=False)[1].nbytes
            if name not in ("u10", "v10"):
                fields.append(
                    dict(
                        name=name, year=int(year), chosen=chosen, z=z, rlo=rlo, clo=clo
                    )
                )
            if out is not None:
                value = np.zeros((len(chosen), len(points)))
                corners = (
                    (r0, c0, (1 - wy) * (1 - wx)),
                    (r0, c1, (1 - wy) * wx),
                    (r1, c0, wy * (1 - wx)),
                    (r1, c1, wy * wx),
                )
                if np.isfinite(z).all():
                    total = np.zeros(len(points))
                    temporary = np.empty_like(value)
                    flat = z.reshape(len(chosen), -1)
                    for rr, cc, w in corners:
                        weight = np.where(w > 1e-12, w, 0)
                        cols = (rr - rlo) * (chi - clo) + cc - clo
                        np.take(flat, cols, axis=1, out=temporary)
                        np.multiply(temporary, weight, out=temporary)
                        np.add(value, temporary, out=value)
                        total += weight
                    np.divide(
                        value, total[None, :], out=value, where=total[None, :] > 0
                    )
                    value[:, total <= 0] = np.nan
                else:
                    total = np.zeros_like(value)
                    for rr, cc, w in corners:
                        a = z[:, rr - rlo, cc - clo]
                        good = np.isfinite(a) & (w[None, :] > 1e-12)
                        value += np.where(good, a * w, 0)
                        total += np.where(good, w, 0)
                    np.divide(value, total, out=value, where=total > 0)
                    value[total <= 0] = np.nan
                value[:, ~(ix & iy)] = np.nan
                out[chosen] = value
        if out is not None:
            fallback[name] = out
    return dict(fields=fields, geometry=geometry), fallback


def prepare(tile, slot=-1, generation=0):
    ring, needed, row, src, config, token, reps = STATE
    k = f"r{tile['row']}_c{tile['col']}"
    timing = {}
    tick = time.perf_counter()
    with land_sources(config) as (grid, rasters):
        mask = land_mask(tile, rasters, "domain" in config.sources)
        q = land_points(tile, mask, grid)
    if len(q) != tile["land_pixels"]:
        raise ValueError(f"Land count differs from planned tile {k}")
    if "point_blocks" in config.sources:
        saved = pd.read_parquet(
            point_block(config, tile), columns=["index", "lat", "lon", "tz"]
        )
        q = q.merge(
            saved, on=["index", "lat", "lon"], how="left", validate="one_to_one"
        )
    elif len(q):
        q["tz"] = timezones(config, q)
    land = q["index"].to_numpy()
    if len(q):
        q = q.dropna(subset=["tz"]).reset_index(drop=True)
    if q.empty:
        return dict(
            meta=dict(key=k, tile=tile, eligible_points=0, period_panels=[]),
            land=land,
            reason_ids=np.array([], dtype=int),
            reason=np.array([], dtype=np.uint8),
        )
    unique(q, ["index"])
    static = src.native_static(q, row.start.year)
    category = np.isin(static["LC"], np.arange(1, 9)) & np.isin(static["KGC"], KGC)
    positions = np.flatnonzero(category)
    p = q.iloc[positions].reset_index(drop=True)
    reason = np.where(category, 0, 2).astype(np.uint8)
    timing["points_static"] = time.perf_counter() - tick
    meta = dict(
        key=k,
        tile=tile,
        land_points=len(land),
        timezone_points=len(q),
        native_points=len(p),
        eligible_points=0,
        timing=timing,
        period_panels=[],
    )
    result = dict(meta=meta, land=land, reason_ids=q["index"].to_numpy(), reason=reason)
    if not len(p):
        return result
    tick = time.perf_counter()
    history, vi, meta["period_panels"] = load_panels(
        p, needed, row, src, k, config.panel_cache or config.output / "panels", token
    )
    panel = history[row.period_id]
    frame = add_previous(panel, history)
    numeric, annual, ok = predictors(frame)
    native = {n: np.asarray(v)[category] for n, v in static.items()}
    kg = np.array([KGC.index(v) for v in native["KGC"]])
    lc = native["LC"]
    good = ok
    selected = p.loc[good].reset_index(drop=True)
    n = int(good.sum())
    reason[positions[~good]] |= 1
    timing["verified_period_context"] = time.perf_counter() - tick
    arrays = ring.arrays(slot, n)
    arrays["numeric"][:] = numeric[good]
    arrays["annual"][:] = annual[good]
    arrays["year"][:] = pd.to_datetime(frame.loc[good, "period_key"]).dt.year.to_numpy()
    result.update(
        points=selected[["index", "lat", "lon"]],
        native_points=p[["index", "lat", "lon"]],
        native_lc=lc,
        native_selected=np.flatnonzero(good),
        positions=positions[good],
        _shared=dict(slot=slot, generation=generation, rows=n),
    )
    meta["eligible_points"] = n
    if n:
        times = pd.date_range(row.start, row.stop, freq="3h", inclusive="left")
        tick = time.perf_counter()
        center = int(abs(vi.dates - pd.Timestamp(row.period_key, tz="UTC")).argmin())
        if config.device == "cpu":
            statics = {
                year: src.native_static(p, year) for year in np.unique(times.year)
            }
            x = compact_input(
                p,
                good,
                row,
                times,
                src.weather(p, times),
                soil(p, row, src),
                vi,
                statics,
                reps,
                numeric,
                annual,
                kg,
                lc,
            )
            arrays["x"][:, : len(reps)] = x
            return result
        for name in ("EVI", "NDVI"):
            np.testing.assert_array_equal(
                panel[name].to_numpy(), vi.values[name][center]
            )
        matching = np.array(
            [int(abs(vi.dates - t.normalize()).argmin()) for t in times]
        )
        dates, matched = np.unique(matching, return_inverse=True)
        evi = vi.values["EVI"][dates][:, good]
        ndvi = vi.values["NDVI"][dates][:, good]
        vi_fields = np.stack([ndvi, evi, evi**2]).astype(np.float32)
        timing["cached_vi"] = time.perf_counter() - tick
        tick = time.perf_counter()
        weather, f = coarse(selected, times, src)
        tc = f["t2m"] - 273.15
        dc = f["d2m"] - 273.15
        vpd = np.maximum(
            0.6108 * np.exp(17.27 * tc / (tc + 237.3))
            - 0.6108 * np.exp(17.27 * dc / (dc + 237.3)),
            0,
        )
        fallback = np.ndarray(
            (n, 3 * len(times)),
            np.float32,
            buffer=ring.memory,
            offset=slot * ring.stride,
            order="F",
        )
        for t in range(len(times)):
            fallback[:, 3 * t] = vpd[t]
            fallback[:, 3 * t + 1] = vpd[t] * f["t2m"][t]
            fallback[:, 3 * t + 2] = np.hypot(f["u10"][t], f["v10"][t])
        del f, tc, dc, vpd
        timing["coarse_and_cpu_nonlinear"] = time.perf_counter() - tick
        tick = time.perf_counter()
        soilbase = src.paths["soil_cache"] / f"gldas_{row.period_id:03d}"
        soilmeta = json.loads(soilbase.with_suffix(".json").read_text())
        soilpath = soilbase.with_suffix(".npy")
        if (
            soilmeta["artifact"] != identity(soilpath)
            or pd.Timestamp(soilmeta["start"]) != row.start
            or pd.Timestamp(soilmeta["stop"]) != row.stop
        ):
            raise ValueError("GLDAS summary mismatch")
        lat = np.asarray(soilmeta["lat"])
        lon = np.asarray(soilmeta["lon"])
        rr = nearest(lat, selected.lat.to_numpy())
        cc = nearest(lon, selected.lon.to_numpy())
        rlo, rhi = int(rr.min()), int(rr.max()) + 1
        clo, chi = int(cc.min()), int(cc.max()) + 1
        a = np.load(soilpath, mmap_mode="r")
        layers = np.array(a[:, rlo:rhi, clo:chi, :], dtype=np.float32)
        inside = (
            selected.lat.between(lat.min() - 0.125, lat.max() + 0.125)
            & selected.lon.between(lon.min() - 0.125, lon.max() + 0.125)
        ).to_numpy()
        zones = p.tz.to_numpy()
        names = list(pd.unique(zones))
        codes = np.array([names.index(v) for v in zones], np.int32)[good]
        phase = np.empty((len(times), len(names)))
        for j, name in enumerate(names):
            local = [t.tz_convert(ZoneInfo(name)) for t in times]
            phase[:, j] = [
                (d.dayofyear + (d.hour + d.minute / 60) / 24)
                / (366 if d.is_leap_year else 365)
                for d in local
            ]
        sine = np.sin(2 * np.pi * phase)
        cosine = np.cos(2 * np.pi * phase)
        calendar = np.stack([sine, cosine, sine * cosine]).astype(np.float32)
        statics = {}
        for year in np.unique(times.year):
            values = (
                native if year == row.start.year else src.native_static(p, int(year))
            )
            statics[int(year)] = np.stack(
                [
                    values[v][good]
                    for v in (
                        "twi",
                        "PH",
                        "Sand",
                        "agb",
                        "SOC",
                        "N",
                        "year_since_disturb",
                    )
                ]
            ).astype(np.float32)
        result["_gpu_native"] = dict(
            weather=weather,
            steps=len(times),
            fallback_columns=3 * len(times),
            vi=vi_fields,
            vi_matched=matched.astype(np.int32),
            soil=layers,
            soil_rows=(rr - rlo).astype(np.int32),
            soil_cols=(cc - clo).astype(np.int32),
            soil_inside=inside.astype(np.uint8),
            calendar=calendar,
            zone_codes=codes,
            static=statics,
            step_year=np.array(times.year, np.int32),
            coords=np.stack([selected.lon, selected.lat]).astype(np.float32),
            kg=kg[good].astype(np.int32),
            lc=(lc[good] - 1).astype(np.int32),
        )
        timing["small_native_sources"] = time.perf_counter() - tick
        meta["payload_bytes"] = n * 3 * len(times) * 4
    return result
