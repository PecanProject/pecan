"""Read-only request planning and immutable source/code binding."""

from pathlib import Path
from contextlib import contextmanager
import fcntl
import shutil
import pandas as pd
from pecan_downscaling.artifacts import identity, sha, digest, read
from pecan_downscaling.calendar import read_periods, selection
from pecan_downscaling.models.bundle import validate
from pecan_downscaling.domain import plan_domain, point_block
from pecan_downscaling.predict.panels import panel_sources


def code_identity():
    root = Path(__file__).parents[1]
    # Only code and resources involved in producing a map invalidate a run.
    files = (
        "artifacts.py", "calendar.py", "configuration.py", "contracts.py", "domain.py",
        "timezones.R", "models/bundle.py", "models/compact.py", "models/inference.py",
        "models/network.py", "predict/engine.py", "predict/gpu_device.py",
        "predict/inputs.py", "predict/panels.py", "predict/point_export_gaps.json",
        "predict/producer.py", "predict/products.py", "predict/shared_ring.py",
        "predict/state.py",
    )
    return {name: sha(root / name) for name in files}


def verify_ledger(records):
    for record in records:
        path = record["path"]
        try:
            current = identity(path)
        except FileNotFoundError as error:
            raise ValueError(f"Bound source disappeared: {path}") from error
        if any(current[k] != record[k] for k in current) or (
            "sha256" in record and sha(path) != record["sha256"]
        ):
            raise ValueError(f"Bound source changed: {path}")


def plan(config):
    """Validate inputs before publishing anything, including on repeated preflight."""
    old = config.destination("manifest.json")
    if old.exists():
        verify_ledger(read(old)["sources"])
    bundle = validate(config.model_bundle)
    needed, wanted = selection(
        read_periods(config.sources["periods"]), config.period_ids
    )
    requested_tiles, vi_tiles, domain_files = plan_domain(config)
    records = {}

    def bind(path, hash_content=False):
        value = identity(path)
        if hash_content:
            value["sha256"] = sha(path)
        records[value["path"]] = value

    for path in domain_files:
        bind(path, path.suffix == ".json")
    for key in (
        "grid",
        "periods",
        "native_lc",
        "kgc",
        "kgc_legend",
        "aqua_ledger",
        "timezones",
    ):
        if key in config.sources:
            bind(
                config.sources[key],
                key
                in (
                    "periods",
                    "kgc_legend",
                    "aqua_ledger",
                    "timezones",
                ),
            )
    years = set(range(2013, 2025))
    native_years = set()
    era_years = set()
    for row in needed.itertuples():
        years.add(
            int(row.hydrological_year)
            if hasattr(row, "hydrological_year") and pd.notna(row.hydrological_year)
            else row.start.year + int(row.start.strftime("%m-%d") >= "07-16")
        )
        base = config.sources["soil_cache"] / f"gldas_{row.period_id:03d}"
        meta = read(base.with_suffix(".json"))
        if (
            meta["artifact"] != identity(base.with_suffix(".npy"))
            or pd.Timestamp(meta["start"]) != row.start
            or pd.Timestamp(meta["stop"]) != row.stop
        ):
            raise ValueError(f"Changed soil cache: {base}")
        for source in meta["sources"]:
            verify_ledger([source])
            records[source["path"]] = source
        bind(base.with_suffix(".npy"))
        bind(base.with_suffix(".json"), True)
        for time in pd.date_range(row.start, row.stop, freq="3h", inclusive="left"):
            native_years.add(time.year)
            era_years.update((time.year, (time + pd.Timedelta(hours=3)).year))
    for year in sorted(years):
        bind(config.sources["annual_covariates"] / f"covariates_{year}.tiff")
    for year in sorted(native_years):
        bind(config.sources["native_covariates"] / f"covariates_{year}.tiff")
    for year in sorted(era_years):
        bind(config.sources["era"] / f"ERA5_{year}.nc")
    cache = config.sources.get("spatial_cache")
    if cache:
        prior = read(cache / "manifest.json")
        hashes = {Path(m["path"]).name: m["sha256"] for m in prior["models"]}
        for o in (1, 2, 3):
            if hashes[f"spatial_o{o}.npz"] != bundle["files"][f"spatial/fold_{o}.npz"]:
                raise ValueError("Spatial cache belongs to different model weights")
        bind(cache / "manifest.json", True)
    for tile in requested_tiles:
        if cache and tile["row"] % 256 == 0 and tile["col"] % 256 == 0:
            path = (
                cache
                / f"continental_cache/spatial/r{tile['row']}_c{tile['col']}_b0.npy"
            )
            if path.with_suffix(".json").exists():
                if read(path.with_suffix(".json"))["artifact"] != identity(path):
                    raise ValueError("Changed spatial cache")
                bind(path)
                bind(path.with_suffix(".json"), True)
        if "point_blocks" in config.sources:
            path = point_block(config, tile)
            stamp = str(path) + ".ok.json"
            if read(stamp)["artifact"] != identity(path):
                raise ValueError(f"Changed points: {path}")
            bind(path)
            bind(stamp, True)
    ledger = read(config.sources["aqua_ledger"])
    start = needed.start.min() - pd.Timedelta(days=64)
    stop = needed.stop.max() + pd.Timedelta(days=64)
    dates = pd.to_datetime(ledger["dates"], format="A%Y%j", utc=True)
    if dates.min() > start + pd.Timedelta(days=16) or dates.max() < stop - pd.Timedelta(
        days=16
    ):
        raise ValueError("Vegetation ledger does not cover requested windows")
    for item in ledger["sources"]:
        if item["tile"] in vi_tiles and start <= pd.Timestamp(item["date"]) <= stop:
            for entry in (item["receipt"], item["raw"], *item["layers"]):
                if identity(entry["path"]) != {
                    k: entry[k] for k in ("path", "size", "mtime_ns")
                }:
                    raise ValueError(f"Changed vegetation input: {entry['path']}")
                if (
                    entry is item["receipt"]
                    and entry.get("sha256")
                    and sha(entry["path"]) != entry["sha256"]
                ):
                    raise ValueError(f"Changed vegetation receipt: {entry['path']}")
                bind(entry["path"], entry is item["receipt"])
    contract = dict(
        schema="pecan-flux-map-run-1",
        code=code_identity(),
        model_id=bundle["id"],
        panel_sources=panel_sources(config.sources, needed, records.values(), ledger),
        panel_cache=str(config.panel_cache) if config.panel_cache else None,
        window=config.window,
        period_ids=list(config.period_ids),
        tiles=requested_tiles,
        device=config.device,
        sources=[records[k] for k in sorted(records)],
        source_locations={k: str(v) for k, v in config.sources.items()},
    )
    contract["token"] = digest(contract)
    if old.exists() and read(old) != contract:
        raise ValueError(
            "Request, sources, model or code changed; use a new output directory"
        )
    return contract, needed, wanted


def budget(config, additional=0):
    existing = (
        sum(p.stat().st_size for p in config.output.rglob("*") if p.is_file())
        if config.output.exists()
        else 0
    )
    if existing + additional > config.max_output_bytes:
        raise ValueError("Configured output storage budget exceeded")
    parent = config.output
    while not parent.exists():
        parent = parent.parent
    if additional > shutil.disk_usage(parent).free:
        raise ValueError("Insufficient free output storage")


@contextmanager
def locked(config):
    config.output.mkdir(parents=True, exist_ok=True)
    with config.destination(".run.lock").open("a") as handle:
        try:
            fcntl.flock(handle, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError as error:
            raise RuntimeError("Another process owns this output directory") from error
        try:
            yield
        finally:
            fcntl.flock(handle, fcntl.LOCK_UN)
