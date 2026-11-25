"""Small explicit environmental fixtures; no acquired data required."""

import numpy as np
import pandas as pd
import pytest
import rasterio
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parents[2] / "inst/python"))

from pecan_downscaling.configuration import Config
from pecan_downscaling.artifacts import write


@pytest.fixture
def run_case(tmp_path, monkeypatch):
    from pecan_downscaling.predict import state

    sources = {}
    inputs = tmp_path / "inputs"
    inputs.mkdir()
    for key in (
        "era",
        "annual_covariates",
        "native_covariates",
    ):
        sources[key] = inputs / key
        sources[key].mkdir()
    for key in (
        "grid",
        "periods",
        "native_lc",
        "kgc",
        "kgc_legend",
    ):
        sources[key] = inputs / key
    raster_profile = dict(
        driver="GTiff",
        width=4,
        height=4,
        count=1,
        dtype="uint8",
        crs="EPSG:4326",
        transform=rasterio.Affine(0.1, 0, -90, 0, -0.1, 45),
    )
    for path in [
        sources["grid"],
        *[
            sources["annual_covariates"] / f"covariates_{y}.tiff"
            for y in range(2013, 2025)
        ],
    ]:
        with rasterio.open(path, "w", **raster_profile) as ds:
            ds.write(np.ones((1, 4, 4), np.uint8))
    for key in ("native_lc", "kgc", "kgc_legend"):
        sources[key].write_bytes(b"source")
    (sources["native_covariates"] / "covariates_2018.tiff").write_bytes(b"static")
    (sources["era"] / "ERA5_2018.nc").write_bytes(b"era")
    sources["soil_cache"] = inputs / "coarse"
    sources["soil_cache"].mkdir()
    sources["aqua_ledger"] = inputs / "ledger.json"
    write(sources["aqua_ledger"], dict(dates=["A2017009", "A2018350"], sources=[]))
    dates = pd.date_range("2018-01-01", periods=6, tz="UTC")
    calendar = pd.DataFrame(
        dict(
            period_id=range(-3, 2),
            period_key=[str(d.date()) for d in dates[:-1]],
            start=dates[:-1],
            stop=dates[1:],
        )
    )
    calendar.to_csv(sources["periods"], index=False)
    from pecan_downscaling.artifacts import identity

    for row in calendar.itertuples():
        base = sources["soil_cache"] / f"gldas_{row.period_id:03d}"
        base.with_suffix(".npy").write_bytes(b"soil cache")
        write(
            base.with_suffix(".json"),
            dict(
                sources=[],
                start=str(row.start),
                stop=str(row.stop),
                artifact=identity(base.with_suffix(".npy")),
            ),
        )
    config = Config(
        tmp_path / "models",
        tmp_path / "output",
        sources,
        dict(row=1, col=1, height=2, width=2),
        (1,),
        "cpu",
        1,
        1,
        10**7,
    )
    monkeypatch.setattr(state, "validate", lambda root: dict(id="model"))
    contract, needed, wanted = state.plan(config)
    return config, contract, needed, wanted
