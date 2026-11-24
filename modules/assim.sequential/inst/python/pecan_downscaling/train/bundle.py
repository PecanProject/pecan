"""Publish the outer-fold models using the prediction backend's bundle contract."""

from pathlib import Path
import shutil
from tempfile import TemporaryDirectory

import numpy as np
import torch

from pecan_downscaling.artifacts import digest, read, sha, write
from pecan_downscaling.models.bundle import (
    SCHEMA,
    feature_contract,
    required_files,
    validate,
)
from pecan_downscaling.train.proxy import load_model, name


def export(run):
    destination = run.root / "model_bundle"
    if destination.exists():
        # The process may have stopped after publishing and before marking the stage.
        if validate(destination)["provenance"]["training_token"] != run.token:
            raise ValueError("Exported model belongs to another training run")
    else:
        with TemporaryDirectory(prefix=".flux-models-", dir=run.root) as scratch:
            root = Path(scratch) / "bundle"
            for folder in ("proxy", "residual", "spatial"):
                (root / folder).mkdir(parents=True)
            for fold in (1, 2, 3):
                _, checkpoint = load_model(run, name(fold))
                prefix = f"fold_{fold}"
                torch.save(
                    {k: v.cpu() for k, v in checkpoint["state_dict"].items()},
                    root / "proxy" / f"{prefix}.pt",
                )
                write(root / "proxy" / f"{prefix}.json", checkpoint["metadata"])
                for target in ("NEE", "LE"):
                    shutil.copyfile(
                        run.root / f"models/residual_o{fold}_j0_{target}.json",
                        root / "residual" / f"{prefix}_{target}.json",
                    )
                model = run.load(f"models/spatial_o{fold}.npz")
                np.savez_compressed(
                    root / "spatial" / f"{prefix}.npz",
                    **{
                        k: model[k]
                        for k in ("cell", "caps", "train_positions", "lc", "coords")
                    },
                )
            payload = dict(
                schema=SCHEMA,
                contract=feature_contract(),
                files={k: sha(root / k) for k in sorted(required_files())},
                provenance=dict(
                    training_token=run.token,
                    prepared_data_id=run.data.manifest["id"],
                    code=run.manifest["code"],
                    recipe=run.manifest["recipe"],
                ),
            )
            write(root / "manifest.json", dict(id=digest(payload), **payload))
            validate(root)
            root.rename(destination)
    # Keep training receipts outside the portable model bundle.
    run.json("validation/bundle.json", dict(model_id=validate(destination)["id"]))


def finish(run):
    run.stage("bundle", ["validation/bundle.json"], lambda: export(run))
    manifest = validate(run.root / "model_bundle")
    if read(run.root / "validation/bundle.json")["model_id"] != manifest["id"]:
        raise ValueError("Training export and bundle disagree")
    return run.root / "model_bundle"
