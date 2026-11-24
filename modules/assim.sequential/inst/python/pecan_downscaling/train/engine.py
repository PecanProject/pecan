"""Train proxy networks, fit residuals, and calibrate held-out spatial errors."""

from contextlib import contextmanager
import fcntl
import os

from pecan_downscaling.train import correct, evaluate, proxy
from pecan_downscaling.train.state import Run
from pecan_downscaling.train.splits import proxy_keys


@contextmanager
def training_directory(path, resume):
    path.mkdir(parents=True, exist_ok=True)
    with (path / "run.lock").open("a") as lock:
        try:
            fcntl.flock(lock, fcntl.LOCK_EX | fcntl.LOCK_NB)
        except BlockingIOError as exc:
            raise RuntimeError(
                f"Another process owns this training run: {path}"
            ) from exc
        entries = set(os.listdir(path)) - {"run.lock"}
        if entries and not resume:
            raise ValueError(
                "Existing training output requires resume or a fresh directory"
            )
        if entries and not entries.intersection({"prepared_data", "manifest.json"}):
            raise ValueError(f"Unrecognized training directory: {path}")
        yield


def check_resources(config):
    if config.cpu_threads > int(os.environ.get("NSLOTS", config.cpu_threads)):
        raise ValueError("Requested CPU threads exceed the allocation")
    if config.device == "cuda":
        import torch
        import cupy

        if (
            not torch.cuda.is_available()
            or torch.cuda.device_count() != 1
            or cupy.cuda.runtime.getDeviceCount() != 1
        ):
            raise ValueError("CUDA training requires one visible allocated GPU")


def train(config):
    run = Run(config)
    run.freeze()
    keys = proxy_keys()
    for key in keys:
        proxy.training_rows(run.data, *key)
    for key in keys:
        run.stage(proxy.name(*key), proxy.outputs(*key), lambda: proxy.train(run, *key))
    run.stage(
        "period_features",
        ["cache/periods.npz", "cache/period_features.npy"],
        lambda: correct.prepare(run),
    )
    for outer in (1, 2, 3):
        for inner in range(4):
            run.stage(
                f"proxy_assemble_{outer}_{inner}",
                [proxy.assembled_path(outer, inner)],
                lambda: proxy.assemble(run, outer, inner),
            )
            run.stage(
                f"residual_{outer}_{inner}",
                correct.outputs(outer, inner),
                lambda: correct.fit(run, outer, inner),
            )
        run.stage(
            f"spatial_{outer}",
            [f"models/spatial_o{outer}.npz", f"predictions/spatial_o{outer}.npz"],
            lambda: evaluate.spatial(run, outer),
        )
    run.stage("report", ["validation/period_scores.json"], lambda: evaluate.report(run))
    return run
