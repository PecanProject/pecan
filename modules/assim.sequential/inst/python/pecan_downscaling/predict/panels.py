import json, fcntl
from pathlib import Path
import pandas as pd
import pyarrow as pa, pyarrow.parquet as pq
from pecan_downscaling.predict.inputs import point_signature, current_panel
from pecan_downscaling.contracts import DYNAMIC
from pecan_downscaling.artifacts import digest, identity, read


def panel_sources(paths, needed, records, ledger):
    # Bump the recipe when panel columns, sampling or aggregation semantics change.
    by_path = {r["path"]: r for r in records}
    dates = pd.to_datetime(ledger["dates"], format="A%Y%j", utc=True)
    gaps = read(Path(__file__).with_name("point_export_gaps.json"))
    layers_by_date = {}
    for item in ledger["sources"]:
        day = item["date"][:10]
        if item["tile"] not in gaps.get(day, ()):
            layers_by_date.setdefault(day, {})[item["tile"]] = {
                layer["path"]: (layer["size"], layer["mtime_ns"], layer.get("sha256"))
                for layer in item["layers"]
            }
    tokens = {}
    for period in needed.itertuples():
        year = getattr(period, "hydrological_year", None)
        if pd.isna(year):
            year = period.start.year + int(period.start.strftime("%m-%d") >= "07-16")
        annual = paths["annual_covariates"] / f"covariates_{int(year)}.tiff"
        soil = paths["soil_cache"] / f"gldas_{period.period_id:03d}.json"
        # The last accumulated ERA observation is stamped at the exclusive stop.
        era = [
            paths["era"] / f"ERA5_{y}.nc"
            for y in range(period.start.year, period.stop.year + 1)
        ]
        support = dates[
            (dates >= period.start - pd.Timedelta(days=64))
            & (dates <= period.stop + pd.Timedelta(days=64))
        ]
        tokens[str(period.period_id)] = digest(
            dict(
                recipe="period-covariates-1",
                vegetation=[
                    (day, layers_by_date.get(day, {}))
                    for day in support.strftime("%Y-%m-%d")
                ],
                key=period.period_key,
                soil=by_path[str(soil.resolve())]["sha256"],
                rasters=[by_path[str(p.resolve())] for p in [annual, *era]],
            )
        )
    return tokens


def load_panels(points, needed, row, src, key, root, token):
    directory = root / key
    directory.mkdir(parents=True, exist_ok=True)
    with (directory / "write.lock").open("a") as lock:
        fcntl.flock(lock, fcntl.LOCK_EX)
        return read_or_build(points, needed, row, src, directory, token)


def read_or_build(points, needed, row, src, directory, token):
    coord = point_signature(points)
    panels, missing, receipts = {}, [], []
    for period in needed.itertuples():
        pid = int(period.period_id)
        path = directory / f"slot_{pid % 5}.parquet"
        sig = dict(
            period=pid,
            start=str(period.start),
            stop=str(period.stop),
            coords=coord,
            source=token[str(pid)],
            soil=identity(src.paths["soil_cache"] / f"gldas_{pid:03d}.npy"),
        )
        hit = False
        if path.exists():
            meta = json.loads(pq.read_schema(path).metadata[b"history_panel"])
            if meta["period"] == pid:
                if meta != sig:
                    raise ValueError(f"Changed period panel inputs: {path}")
                columns = None if pid == row.period_id else ["index", *DYNAMIC]
                panels[pid] = pq.read_table(
                    path,
                    columns=columns,
                    use_threads=False,
                    page_checksum_verification=True,
                ).to_pandas(use_threads=False)
                hit = True
        if not hit:
            missing.append((period, path, sig))
        receipts.append(dict(period=pid, path=str(path), reused=hit))
    active = {row.period_id, *(r.period_id for r, _, _ in missing)}
    vi = src.vegetation(points, needed[needed.period_id.isin(active)])
    for period, path, sig in missing:
        panel = current_panel(points, vi, period, src, {})[0]
        table = pa.Table.from_pandas(panel, preserve_index=False)
        table = table.replace_schema_metadata(
            {**table.schema.metadata, b"history_panel": json.dumps(sig).encode()}
        )
        temp = path.with_suffix(".partial")
        pq.write_table(
            table,
            temp,
            compression="zstd",
            compression_level=1,
            write_page_checksum=True,
        )
        temp.replace(path)
        panels[period.period_id] = panel
    return panels, vi, receipts
