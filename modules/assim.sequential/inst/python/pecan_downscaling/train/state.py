"""Frozen training requests and output-verified stage completion."""

from pathlib import Path
import os
import time
import numpy as np
from pecan_downscaling.artifacts import (
    digest,
    read,
    write,
    sha,
    receipt,
    require,
    save_npz,
)
from pecan_downscaling.train.data import Dataset
from pecan_downscaling.train.recipe import contract


def code_identity():
    component = Path(__file__).resolve().parents[1]
    files = [
        *sorted((component / "train").glob("*.py")),
        component / "artifacts.py",
        component / "contracts.py",
        component / "models/bundle.py",
        component / "models/network.py",
    ]
    return {str(p.relative_to(component)): sha(p) for p in files}


class Run:
    def __init__(self, config):
        self.config = config
        self.root = config.output.resolve()
        self.data = Dataset(config.prepared_data)
        payload = dict(
            schema="pecan-flux-training-run-1",
            prepared_data=dict(path=str(self.data.root), id=self.data.manifest["id"]),
            recipe=contract(),
            code=code_identity(),
            resources=dict(device=config.device, cpu_threads=config.cpu_threads),
        )
        self.token = digest(payload)
        self.manifest = dict(token=self.token, **payload)

    def freeze(self):
        path = self.root / "manifest.json"
        if path.exists():
            if read(path) != self.manifest:
                raise ValueError(
                    "Training code, data or request changed; use a new output directory"
                )
        else:
            existing = list(self.root.iterdir()) if self.root.exists() else []
            if any(p.name not in {"run.lock", "prepared_data"} for p in existing):
                raise ValueError("Unbound training outputs; choose a new directory")
            write(path, self.manifest)

    def path(self, name):
        path = (self.root / name).resolve()
        if not path.is_relative_to(self.root):
            raise ValueError(f"Output outside training run: {name}")
        path.parent.mkdir(parents=True, exist_ok=True)
        return path

    def json(self, name, value):
        path = self.path(name)
        write(path, value)
        receipt(path, self.token)

    def save(self, name, **arrays):
        save_npz(self.path(name), self.token, **arrays)

    def array_save(self, name, value):
        path = self.path(name)
        partial = path.with_name(path.name + f".partial.{os.getpid()}")
        with partial.open("wb") as handle:
            np.save(handle, value, allow_pickle=False)
        partial.replace(path)
        receipt(path, self.token)

    def load(self, name):
        path = self.root / name
        require(path, self.token)
        with np.load(path, allow_pickle=False) as archive:
            return {key: archive[key] for key in archive.files}

    def array(self, name):
        path = self.root / name
        require(path, self.token)
        return np.load(path, mmap_mode="r", allow_pickle=False)

    def verify_stage(self, name, outputs):
        marker = self.root / f"stages/{name}.json"
        record = read(marker)
        if (
            record.get("status") != "complete"
            or record.get("token") != self.token
            or record.get("outputs") != outputs
        ):
            raise ValueError(f"Stage output contract changed: {name}")
        for output in outputs:
            require(self.root / output, self.token)

    def stage(self, name, outputs, function):
        marker = self.root / f"stages/{name}.json"
        if marker.exists():
            self.verify_stage(name, outputs)
            print(f"REUSE {name}", flush=True)
            return False
        start = time.perf_counter()
        print(f"START {name}", flush=True)
        function()
        for output in outputs:
            require(self.root / output, self.token)
        write(
            marker,
            dict(
                status="complete",
                outputs=outputs,
                token=self.token,
                seconds=time.perf_counter() - start,
            ),
        )
        print(f"COMPLETE {name} {time.perf_counter() - start:.2f}s", flush=True)
        return True
