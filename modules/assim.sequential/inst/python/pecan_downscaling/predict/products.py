"""Seven-band, georeferenced period flux products."""

from contextlib import ExitStack
import numpy as np
import rasterio
from rasterio.windows import Window
from pecan_downscaling.artifacts import require, save_npz, receipt, write

BANDS = (
    "mean",
    "fold_std",
    "fold1_mean",
    "fold2_mean",
    "fold3_mean",
    "eligible",
    "missing_reason",
)
TARGETS = ("NEE", "LE")


def block(config, row, tile):
    return config.destination(
        f"blocks/p{row.period_id:03d}/r{tile['row']}_c{tile['col']}.npz"
    )


def store(config, row, tile, payload, pred, token):
    path = block(config, row, tile)
    ids = (
        payload["points"]["index"].to_numpy()
        if pred is not None
        else np.empty(0, np.int64)
    )
    write(
        path.with_suffix(".json"),
        dict(token=token, panels=payload["meta"]["period_panels"]),
    )
    receipt(path.with_suffix(".json"), token)
    save_npz(
        path,
        token,
        land=payload["land"],
        reason_ids=payload["reason_ids"],
        reason=payload["reason"],
        pixel=ids,
        fold_mean=pred if pred is not None else np.empty((3, 0, 2), np.float32),
    )


def path_for(config, row, target):
    return config.destination(f"products/period/{target}_{row.period_id:03d}.tif")


def array_for(z, tile, width, target):
    def cells(ids):
        return ids // width - tile["row"], ids % width - tile["col"]

    a = np.full((7, tile["height"], tile["width"]), np.nan, np.float32)
    r, c = cells(z["land"])
    a[5, r, c] = 0
    a[6, r, c] = 4
    r, c = cells(z["reason_ids"])
    a[6, r, c] = z["reason"]
    r, c = cells(z["pixel"])
    fold = z["fold_mean"][:, :, target]
    a[2:5, r, c] = fold
    a[0, r, c] = fold.mean(0)
    a[1, r, c] = fold.std(0)
    a[5, r, c] = 1
    return a


def products(config, manifest, row, create):
    token = manifest["token"]
    w = config.window
    with ExitStack() as stack:
        grid = stack.enter_context(rasterio.open(config.sources["grid"]))
        transform = grid.window_transform(
            Window(w["col"], w["row"], w["width"], w["height"])
        )
        profile = dict(
            driver="GTiff",
            width=w["width"],
            height=w["height"],
            count=7,
            dtype="float32",
            crs=grid.crs,
            transform=transform,
            nodata=np.nan,
            tiled=True,
            sparse_ok=True,  # Unwritten water blocks read back as NoData.
            blockxsize=256,
            blockysize=256,
            compress="deflate",
            predictor=3,
            BIGTIFF="IF_SAFER",
        )
        files = []
        for target in TARGETS:
            path = path_for(config, row, target)
            tags = dict(
                target=target,
                period_id=str(row.period_id),
                start_utc=str(row.start),
                stop_utc_exclusive=str(row.stop),
                model_token=token,
                units="W m-2" if target == "LE" else "legacy Yang NEE model convention",
                missing_reason_bits="1:proxy/context;2:native category;4:timezone",
            )
            if create:
                path.parent.mkdir(parents=True, exist_ok=True)
                f = stack.enter_context(
                    rasterio.open(path.with_suffix(".partial.tif"), "w", **profile)
                )
                f.descriptions = BANDS
                f.update_tags(**tags)
            else:
                require(path, token)
                f = stack.enter_context(rasterio.open(path))
                if (f.shape, f.crs, f.transform, f.descriptions) != (
                    (w["height"], w["width"]),
                    grid.crs,
                    transform,
                    BANDS,
                ) or any(f.tags().get(k) != v for k, v in tags.items()):
                    raise ValueError(f"Invalid period product: {path}")
            files.append(f)
        for tile in manifest["tiles"]:
            path = block(config, row, tile)
            require(path, token)
            require(path.with_suffix(".json"), token)
            win = Window(
                tile["col"] - w["col"],
                tile["row"] - w["row"],
                tile["width"],
                tile["height"],
            )
            with np.load(path) as z:
                for i, f in enumerate(files):
                    a = array_for(z, tile, grid.width, i)
                    if create:
                        f.write(a, window=win)
                    else:
                        np.testing.assert_array_equal(f.read(window=win), a)
    if create:
        for target in TARGETS:
            path = path_for(config, row, target)
            path.with_suffix(".partial.tif").replace(path)
            receipt(path, token)
