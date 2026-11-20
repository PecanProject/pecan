"""Portable, content-verified model bundles independent of training releases."""

from pathlib import Path
from pecan_downscaling.artifacts import read, sha, digest
from pecan_downscaling.contracts import TARGETS, NATIVE, KGC, NUMERIC

SCHEMA = "sipnet-period-residual-1"


def prepared_contract():
    return dict(
        targets=TARGETS,
        native=NATIVE,
        kgc=KGC,
        proxy_numeric=NUMERIC,
        native_lc=list(range(1, 9)),
        proxy_lc=list(range(12)),
        native_step_hours=3,
        spatial_neighbors=20,
        folds=[1, 2, 3],
        fold_spread="population_standard_deviation",
    )


def feature_contract():
    return dict(
        **prepared_contract(),
        sequence_steps=128,
        forest_features=4220,
        spatial_months=12,
    )


def required_files():
    return {
        name
        for fold in (1, 2, 3)
        for name in [
            f"proxy/fold_{fold}.pt",
            f"proxy/fold_{fold}.json",
            f"spatial/fold_{fold}.npz",
            *[f"residual/fold_{fold}_{target}.json" for target in TARGETS],
        ]
    }


def validate(root):
    root = Path(root).resolve()
    manifest = read(root / "manifest.json")
    if (
        manifest.get("schema") != SCHEMA
        or manifest.get("contract") != feature_contract()
    ):
        raise ValueError("Incompatible model feature contract")
    payload = {k: v for k, v in manifest.items() if k != "id"}
    if digest(payload) != manifest.get("id"):
        raise ValueError("Model manifest digest mismatch")
    if set(manifest.get("files", {})) != required_files():
        raise ValueError("Incomplete model bundle inventory")
    for name, expected in manifest["files"].items():
        path = (root / name).resolve()
        if not path.is_relative_to(root) or not path.is_file():
            raise ValueError(f"Missing model artifact: {name}")
        if sha(path) != expected:
            raise ValueError(f"Model artifact hash mismatch: {name}")
    return manifest
