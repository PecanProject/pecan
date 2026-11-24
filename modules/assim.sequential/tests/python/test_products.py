import pytest
import rasterio
from pecan_downscaling.predict.products import products, path_for
from pecan_downscaling.artifacts import receipt
from test_resume import publish_blocks


@pytest.mark.parametrize("excluded", [False, True])
def test_period_inventory_and_timezone_support(run_case, excluded):
    cfg, manifest, _, wanted = run_case
    row = next(wanted.itertuples())
    publish_blocks(run_case, excluded)
    products(cfg, manifest, row, True)
    products(cfg, manifest, row, False)
    path = path_for(cfg, row, "LE")
    with rasterio.open(path) as ds:
        assert (
            ds.count == 7
            and (ds.read(6) == (0 if excluded else 1)).all()
            and (ds.read(7) == (4 if excluded else 0)).all()
        )
    path.rename(cfg.output / "missing.tif")
    with pytest.raises(ValueError, match="Missing"):
        products(cfg, manifest, row, False)


@pytest.mark.parametrize("fault", ["block", "qa", "token"])
def test_incomplete_blocks(run_case, fault):
    cfg, manifest, _, wanted = run_case
    row = next(wanted.itertuples())
    p = publish_blocks(run_case)
    if fault == "token":
        receipt(p, "old")
    else:
        if fault == "qa":
            p = p.with_suffix(".json")
        p.rename(cfg.output / "removed")
    with pytest.raises(ValueError):
        products(cfg, manifest, row, True)


def test_cached_mask_skips_water_blocks_and_preserves_nodata(run_case):
    from dataclasses import replace
    import numpy as np
    from pecan_downscaling.artifacts import identity, write
    from pecan_downscaling.domain import plan_domain
    from pecan_downscaling.predict.products import store

    cfg, _, _, wanted = run_case
    cfg = replace(cfg, window=dict(row=0, col=0, height=4, width=520))
    annual = [
        cfg.sources["annual_covariates"] / f"covariates_{y}.tiff"
        for y in range(2013, 2025)
    ]
    with rasterio.open(cfg.sources["grid"]) as src:
        profile = dict(src.profile, width=520)
    mask = np.zeros((4, 520), np.uint8)
    mask[0, 3] = mask[2, 519] = 1
    for path in [cfg.sources["grid"], *annual]:
        with rasterio.open(path, "w", **profile) as ds:
            ds.write(mask, 1)
    expected, vi, _ = plan_domain(cfg)
    domain = cfg.output.parent / "domain"
    domain.mkdir()
    cfg.sources["domain"] = domain
    with rasterio.open(domain / "domain.tif", "w", **profile) as ds:
        ds.write(mask, 1)
    record = dict(
        artifact=identity(domain / "domain.tif"), sources=[identity(p) for p in annual]
    )
    write(domain / "domain.json", record)
    tiles, actual_vi, _ = plan_domain(cfg)
    assert (
        tiles == expected and vi == actual_vi and [t["col"] for t in tiles] == [0, 512]
    )
    record.update(
        land_pixels=2,
        tiles=[
            dict(
                row=t["row"],
                col=t["col"],
                height=t["height"],
                width=t["width"],
                pixels=t["land_pixels"],
            )
            for t in expected
        ],
    )
    write(domain / "domain.json", record)
    write(
        domain / "modis_tiles.json",
        dict(domain=record["artifact"], tiles={v: 2 // len(vi) for v in vi}),
    )
    tiles, actual_vi, _ = plan_domain(cfg)
    assert tiles == expected and vi == actual_vi
    row = next(wanted.itertuples())
    for tile, pixel in zip(tiles, [3, 1559]):
        store(
            cfg,
            row,
            tile,
            dict(
                land=np.array([pixel]),
                reason_ids=np.empty(0, dtype=int),
                reason=[],
                meta=dict(period_panels=[]),
            ),
            None,
            "token",
        )
    manifest = dict(token="token", tiles=tiles)
    products(cfg, manifest, row, True)
    products(cfg, manifest, row, False)
    for target in ("NEE", "LE"):
        with rasterio.open(path_for(cfg, row, target)) as src:
            values = src.read()
            assert np.isnan(values[:, mask == 0]).all()
            assert (values[6, mask == 1] == 4).all()
    record["sources"].pop()
    write(domain / "domain.json", record)
    with pytest.raises(ValueError, match="annual land-cover"):
        plan_domain(cfg)
