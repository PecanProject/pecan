"""Reduce station member Parquet partitions into labelled Yang period targets."""

import h5py
import numpy as np
import pandas as pd

MEMBERS = [f"ens{i:02d}" for i in range(1, 26)]
ROUTES = [f"route{i:02d}" for i in range(1, 26)]


def reduce_members(root, parts, target, ids, times, *, loaded=None):
    sums = np.zeros((len(ids), len(MEMBERS)))
    counts = np.zeros((*sums.shape, 3), dtype=np.uint16)
    if sorted(part["year"] for part in parts) != sorted(times.year.unique()):
        raise ValueError("Missing or duplicate year partition")
    for part in parts:
        path = root / part["name"] / f"{target}.parquet"
        # Reindex both axes explicitly, including requested sites with no finite member.
        if loaded is None:
            frame = pd.read_parquet(
                path, columns=["index", "utc", *MEMBERS, *ROUTES]
            ).set_index(["index", "utc"])
        else:
            frame = loaded[part["year"]]
        clock = times[times.year == part["year"]]
        keys = pd.MultiIndex.from_product([ids, clock], names=["index", "utc"])
        if (
            not frame.index.is_unique
            or len(keys) != len(frame)
            or not keys.isin(frame.index).all()
        ):
            raise ValueError(f"Invalid station or time keys: {path}")
        frame = frame.loc[keys]
        shape = (len(ids), len(clock), len(MEMBERS))
        values = frame[MEMBERS].to_numpy(dtype=float).reshape(shape)
        route = frame[ROUTES].to_numpy().reshape(shape)
        finite = np.isfinite(values)
        if not np.isin(route, [0, 1, 2, 3]).all() or not np.array_equal(
            finite, route != 0
        ):
            raise ValueError(f"Inconsistent member values and route codes: {path}")
        sums += np.where(finite, values, 0).sum(axis=1)
        for i in range(3):
            counts[:, :, i] += (route == i + 1).sum(axis=1, dtype=np.uint16)
    support = counts.sum(axis=2)
    values = np.divide(sums, support, out=np.full_like(sums, np.nan), where=support > 0)
    # argmax picks the first route on a tie: direct tower, corrected, raw.
    dominant = np.where(support > 0, counts.argmax(axis=2) + 1, 0)
    return values.T, support.T, dominant.T


def write_periods(inputs, parts, root):
    sites, periods = inputs.sites["index"], inputs.periods
    dest = root / "targets"
    dest.mkdir()
    partitions = {}
    for part in parts:
        partitions.setdefault((part["period"], part["chunk"]), []).append(part)
    chunks = sites.groupby((sites - 1) // 500 + 1).indices
    for target in ("NEE", "LE"):
        with h5py.File(dest / f"{target.lower()}.h5", "w") as h:
            for name, dtype in (
                ("values", "<f4"),
                ("support_counts", "<u2"),
                ("route_codes", "u1"),
            ):
                h.create_dataset(
                    name,
                    (len(periods), 25, len(sites)),
                    dtype=dtype,
                    chunks=(1, 25, min(512, len(sites))),
                    compression="gzip",
                    compression_opts=4,
                )
            for name, labels in (
                ("period_keys", periods.period_key),
                ("ensemble_ids", MEMBERS),
                ("site_ids", sites),
            ):
                h[name] = np.asarray(list(map(str, labels)), dtype=h5py.string_dtype())
            h["expected_available_steps"] = periods.expected_3hour_steps.to_numpy(
                dtype=int
            )
            h["full_window_fraction"] = periods.full_window_fraction
            h.attrs.update(
                target=target,
                axis_order="period,ensemble,site",
                window_convention="UTC [t,t+3h)",
                route_legend="0=missing,1=direct_ec,2=residual_model,3=raw_model",
            )
            stats = {
                name: []
                for name in ("finite_count", "value_min", "value_max", "value_mean")
            }
            for i, p in enumerate(periods.itertuples()):
                times = pd.date_range(p.start, p.stop, freq="3h", inclusive="left")
                for chunk, cols in chunks.items():
                    reduced = reduce_members(
                        root / "members",
                        partitions[p.period_id, chunk],
                        target,
                        sites.iloc[cols],
                        times,
                    )
                    where = (
                        slice(cols[0], cols[-1] + 1)
                        if np.all(np.diff(cols) == 1)
                        else cols
                    )
                    for key, array in zip(
                        ("values", "support_counts", "route_codes"), reduced
                    ):
                        h[key][i, :, where] = array
                frame = h["values"][i]
                valid = frame[np.isfinite(frame)].astype(float)
                stats["finite_count"].append(len(valid))
                for op in ("min", "max", "mean"):
                    stats["value_" + op].append(
                        getattr(valid, op)() if len(valid) else np.nan
                    )
            for name, values in stats.items():
                h[name] = values
