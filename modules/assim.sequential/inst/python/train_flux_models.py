"""Train and export three-fold NEE and LE models from PEcAn station products."""

import argparse
import os
from pathlib import Path

from pecan_downscaling.train.configuration import Config
from pecan_downscaling.train.prepare import Inputs, prepare


def train_flux_models(config, outdir, resume=False):
    inputs = Inputs(config)
    root = Path(outdir).resolve()
    for source in (
        inputs.targets,
        inputs.annual_covariates,
        inputs.folds,
        inputs.config,
        inputs.stations,
        inputs.calendar,
        *inputs.station.paths.values(),
    ):
        if root.is_relative_to(source) or source.is_relative_to(root):
            raise ValueError(f"Training output overlaps input: {source}")
    resources = inputs.resources
    if not isinstance(resources, dict) or set(resources) - {"device", "cpu_threads"}:
        raise ValueError("Training resources accept device and cpu_threads")
    config = Config(root / "prepared_data", root, **resources)
    # CPU external-memory XGBoost can probe a visible GPU even with device=cpu.
    # Select visibility in this private R-launched process before loading either ML library.
    if config.device == "cpu":
        os.environ["CUDA_VISIBLE_DEVICES"] = ""
    from pecan_downscaling.train.bundle import finish
    from pecan_downscaling.train.engine import check_resources, train, training_directory

    check_resources(config)
    with training_directory(root, resume):
        prepare(inputs, config.prepared_data, resume)
        return finish(train(config))


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("config")
    parser.add_argument("outdir")
    parser.add_argument("--resume", action="store_true")
    args = parser.parse_args()
    print(train_flux_models(args.config, args.outdir, args.resume))
