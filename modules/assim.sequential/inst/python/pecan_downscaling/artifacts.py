"""Immutable identities and atomic run artifacts; no import-time filesystem writes."""

import hashlib
import json
import os
from pathlib import Path
import numpy as np


def sha(path):
    result = hashlib.sha256()
    with Path(path).open("rb") as handle:
        for data in iter(lambda: handle.read(1024 * 1024), b""):
            result.update(data)
    return result.hexdigest()


def identity(path):
    path = Path(path).resolve()
    stat = path.stat()
    return dict(path=str(path), size=stat.st_size, mtime_ns=stat.st_mtime_ns)


def read(path):
    return json.loads(Path(path).read_text())


def encode(value):
    if isinstance(value, Path):
        return str(value)
    if isinstance(value, np.ndarray):
        return value.tolist()
    if isinstance(value, np.generic):
        return value.item()
    raise TypeError(type(value).__name__)


def digest(value):
    return hashlib.sha256(
        json.dumps(value, sort_keys=True, default=encode, allow_nan=False).encode()
    ).hexdigest()


def write(path, value):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    partial = path.with_name(path.name + f".partial.{os.getpid()}")
    partial.write_text(
        json.dumps(value, indent=2, default=encode, allow_nan=False) + "\n"
    )
    partial.replace(path)


def receipt(path, token):
    write(str(path) + ".ok.json", dict(token=token, artifact=identity(path)))


def valid(path, token):
    path = Path(path)
    stamp = Path(str(path) + ".ok.json")
    if not path.exists() or not stamp.exists():
        return False
    record = read(stamp)
    if record.get("token") != token or record.get("artifact") != identity(path):
        raise ValueError(f"Stale or damaged artifact: {path}")
    return True


def require(path, token):
    if not valid(path, token):
        raise ValueError(f"Missing artifact or receipt: {path}")


def save_npz(path, token, **arrays):
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    partial = path.with_name(path.name + f".partial.{os.getpid()}")
    with partial.open("wb") as handle:
        np.savez_compressed(handle, **arrays)
    partial.replace(path)
    receipt(path, token)
