"""Generate Yang station features, three-hour members and period targets."""

import argparse
from pathlib import Path
import subprocess
import tempfile

from pecan_downscaling.artifacts import write
from pecan_downscaling.targets.periods import write_periods
from pecan_downscaling.targets.stations import TargetInputs, write_features


def prepare(config, outdir, models, rscript):
    inputs = TargetInputs(config)
    outdir = Path(outdir).resolve()
    if outdir.exists():
        raise FileExistsError(f"Target output already exists: {outdir}")
    if models:
        models = Path(models).resolve(strict=True)
        if not models.is_dir():
            raise ValueError(
                f"Expected a directory with residual model bundles: {models}"
            )
    for source in [*inputs.paths.values(), *([models] if models else [])]:
        if source.is_dir() and outdir.is_relative_to(source):
            raise ValueError(f"Choose an output outside the input directory: {source}")
    outdir.parent.mkdir(exist_ok=True, parents=True)
    with tempfile.TemporaryDirectory(
        prefix=".flux-targets-", dir=outdir.parent
    ) as directory:
        scratch = Path(directory)
        stage = scratch / "output"
        stage.mkdir()
        parts = write_features(inputs, stage / "features")
        spec = scratch / "input.json"
        write(spec, dict(paths=inputs.paths, partitions=parts))
        script = Path(__file__).parent / "pecan_downscaling/targets/residuals.R"
        subprocess.run(
            [
                rscript,
                "--vanilla",
                script,
                spec,
                stage,
                scratch,
                *([models] if models else []),
            ],
            check=True,
        )
        write_periods(inputs, parts, stage)
        stage.rename(outdir)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("config")
    parser.add_argument("outdir")
    parser.add_argument("--rscript", required=True)
    parser.add_argument("--models")
    args = parser.parse_args()
    prepare(args.config, args.outdir, args.models, args.rscript)
