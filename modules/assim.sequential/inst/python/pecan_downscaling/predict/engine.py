"""CPU producers feed one GPU consumer with bounded shared buffers."""

from concurrent.futures import ProcessPoolExecutor, wait, FIRST_COMPLETED
from multiprocessing import get_context
import hashlib
from types import SimpleNamespace
from pecan_downscaling.predict.inputs import point_signature
import numpy as np
import pandas as pd
from pecan_downscaling.artifacts import valid, require, write, read, receipt
from pecan_downscaling.models.compact import layout, compile_models
from pecan_downscaling.models.inference import Models
from pecan_downscaling.predict import producer, gpu_device
from pecan_downscaling.predict.shared_ring import Ring
from pecan_downscaling.predict.state import plan, locked, budget, verify_ledger
from pecan_downscaling.predict.products import block, store, products, path_for, TARGETS


def run_period(config, contract, row, needed, models):
    token = contract["token"]
    todo = []
    for tile in contract["tiles"]:
        path = block(config, row, tile)
        if valid(path, token):
            require(path.with_suffix(".json"), token)
        else:
            todo.append(tile)
    if not todo:
        return
    raw, reps, mapping = layout(row, config.model_bundle)
    forests = compile_models(raw, reps, mapping, config.device)
    if config.device == "cuda":
        import cupy as cp

        runtime = SimpleNamespace(cp=cp, reps=reps, capacity=0)
    workers = min(config.workers, len(todo))
    ring = Ring(
        workers,
        max(3 * 128, len(reps)),
        maximum_rows=max(t["width"] * t["height"] for t in todo),
    )
    try:
        with ProcessPoolExecutor(
            workers,
            mp_context=get_context("spawn"),
            initializer=producer.initialize,
            initargs=(
                ring,
                needed,
                SimpleNamespace(**row._asdict()),
                config,
                contract["panel_sources"],
                reps,
            ),
        ) as pool:
            tiles = iter(todo)
            pending = {}

            def submit(slot):
                tile = next(tiles, None)
                if tile is not None:
                    pending[pool.submit(producer.prepare, tile, slot)] = (tile, slot)

            for slot in range(workers):
                submit(slot)
            while pending:
                for future in wait(pending, return_when=FIRST_COMPLETED).done:
                    tile, slot = pending.pop(future)
                    p = future.result()
                    n = p["meta"]["eligible_points"]
                    pred = None
                    if n:
                        p.update(ring.arrays(slot, n))
                        pred = models.proxy_arrays(p["numeric"], p["annual"], p["year"])
                        if config.device == "cuda":
                            x, _ = gpu_device.build(runtime, p)
                        else:
                            x = p["x"][:, : len(reps)]
                        month = pd.Timestamp(row.period_key).month - 1
                        ix = p["native_selected"]
                        coords = p["points"][["lat", "lon"]].to_numpy()
                        bias = None
                        if (
                            "spatial_cache" in config.sources
                            and tile["width"] == tile["height"] == 256
                            and tile["col"] % 256 == tile["row"] % 256 == 0
                        ):
                            cache = config.sources["spatial_cache"]
                            path = (
                                cache
                                / f"continental_cache/spatial/r{tile['row']}_c{tile['col']}_b0.npy"
                            )
                            if path.with_suffix(".json").exists():
                                signature = dict(
                                    token=read(cache / "manifest.json")["token"],
                                    points=point_signature(p["native_points"]),
                                    lc=hashlib.sha256(
                                        p["native_lc"].tobytes()
                                    ).hexdigest(),
                                )
                                if (
                                    read(path.with_suffix(".json"))["signature"]
                                    != signature
                                ):
                                    raise ValueError(
                                        "Different spatial cache coordinates or classes"
                                    )
                                bias = np.load(path, mmap_mode="r")[month][:, ix]
                        for fold in (1, 2, 3):
                            for t, target in enumerate(TARGETS):
                                v = forests[fold, target].inplace_predict(x)
                                pred[fold - 1, :, t] += (
                                    v.get() if config.device == "cuda" else v
                                )
                            pred[fold - 1] -= (
                                bias[fold - 1]
                                if bias is not None
                                else models.spatial[fold].query(
                                    coords, p["native_lc"][ix]
                                )[month]
                            )
                        if not np.isfinite(pred).all():
                            raise ValueError("Nonfinite eligible prediction")
                    store(config, row, tile, p, pred, token)
                    del p
                    submit(slot)
    finally:
        ring.close()


def execute(command, config, resume=False):
    contract, needed, wanted = plan(config)
    token = contract["token"]
    if command == "preflight":
        return dict(status="ready", token=token, tiles=len(contract["tiles"]))
    with locked(config):
        manifest = config.destination("manifest.json")
        if command == "report" and not manifest.exists():
            raise ValueError("Missing run manifest")
        if manifest.exists() and read(manifest) != contract:
            raise ValueError("Run manifest changed before locking")
        completed = valid(config.destination("COMPLETE.json"), token)
        if command == "run":
            if manifest.exists() and not resume:
                raise ValueError("Use --resume for an existing run")
            if not manifest.exists():
                write(manifest, contract)
            budget(
                config,
                config.window["height"]
                * config.window["width"]
                * 7
                * 4
                * 2
                * len(wanted),
            )
            if not completed:
                models = Models(config.model_bundle, config.device, config.cpu_threads)
        inventory = []
        for row in wanted.itertuples():
            if command == "run" and not completed:
                run_period(
                    config,
                    contract,
                    row,
                    needed[needed.period_id.between(row.period_id - 4, row.period_id)],
                    models,
                )
                if not all(valid(path_for(config, row, t), token) for t in TARGETS):
                    products(config, contract, row, True)
            products(config, contract, row, False)
            for target in TARGETS:
                inventory.append(dict(
                    period_id=int(row.period_id),
                    variable=target,
                    start_utc=row.start.isoformat(),
                    stop_utc_exclusive=row.stop.isoformat(),
                    path=str(path_for(config, row, target)),
                ))
        verify_ledger(contract["sources"])
        result = dict(status="complete", token=token, products=inventory)
        path = config.destination("COMPLETE.json")
        if valid(path, token):
            if read(path) != result:
                raise ValueError("Completion inventory mismatch")
        else:
            write(path, result)
            receipt(path, token)
        return result
