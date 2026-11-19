"""Bounded original-grid land masks and explicit timezone exclusions."""

from contextlib import ExitStack, contextmanager
from pathlib import Path
import subprocess
import numpy as np
import pandas as pd
import rasterio
from rasterio.windows import Window
from zoneinfo import ZoneInfo
from pecan_downscaling.artifacts import digest, identity, read
from rasterio.warp import transform


@contextmanager
def land_sources(config):
    with ExitStack() as stack:
        grid = stack.enter_context(rasterio.open(config.sources["grid"]))
        domain = config.sources.get("domain")
        paths = (
            [domain / "domain.tif"]
            if domain
            else [
                config.sources["annual_covariates"] / f"covariates_{year}.tiff"
                for year in range(2013, 2025)
            ]
        )
        rasters = [stack.enter_context(rasterio.open(path)) for path in paths]
        if any(
            (src.shape, src.crs, src.transform)
            != (grid.shape, grid.crs, grid.transform)
            for src in rasters
        ):
            raise ValueError("Land mask/reference grid mismatch")
        yield grid, rasters


def land_mask(tile, rasters, cached):
    window = Window(tile["col"], tile["row"], tile["width"], tile["height"])
    if cached:
        return rasters[0].read(1, window=window) == 1
    mask = np.zeros((tile["height"], tile["width"]), bool)
    for source in rasters:
        values = source.read(1, window=window, masked=True)
        if not np.isin(values.compressed(), np.arange(12)).all():
            raise ValueError("Unknown annual land-cover code")
        mask |= values.filled(0) > 0
    return mask


def land_points(tile, mask, grid):
    rows, cols = np.nonzero(mask)
    rows += tile["row"]
    cols += tile["col"]
    lon, lat = rasterio.transform.xy(grid.transform, rows, cols)
    return pd.DataFrame(dict(index=rows * grid.width + cols, lat=lat, lon=lon))


def plan_domain(config):
    w = config.window
    bottom, right = w["row"] + w["height"], w["col"] + w["width"]
    domain = config.sources.get("domain")
    files = []
    vi_tiles = set()
    tiles = []
    with land_sources(config) as (grid, rasters):
        if (
            grid.crs != rasterio.crs.CRS.from_epsg(4326)
            or grid.transform.b
            or grid.transform.d
        ):
            raise ValueError("A north-up EPSG:4326 reference grid is required")
        if bottom > grid.height or right > grid.width:
            raise ValueError("Requested window exceeds reference grid")
        if domain:
            artifact = identity(domain / "domain.tif")
            metadata = read(domain / "domain.json")
            annual = [
                identity(config.sources["annual_covariates"] / f"covariates_{y}.tiff")
                for y in range(2013, 2025)
            ]
            if metadata["artifact"] != artifact or metadata["sources"] != annual:
                raise ValueError("Changed land mask or annual land-cover sources")
            files.extend([domain / "domain.tif", domain / "domain.json"])
            if (
                grid.shape == (w["height"], w["width"])
                and (domain / "modis_tiles.json").exists()
            ):
                inventory = read(domain / "modis_tiles.json")
                if inventory["domain"] != artifact:
                    raise ValueError(
                        "MODIS tile inventory belongs to a different land mask"
                    )
                tiles = [
                    dict(
                        row=t["row"],
                        col=t["col"],
                        height=t["height"],
                        width=t["width"],
                        land_pixels=t["pixels"],
                    )
                    for t in metadata["tiles"]
                ]
                if (
                    not tiles
                    or sum(t["land_pixels"] for t in tiles) != metadata["land_pixels"]
                    or sum(inventory["tiles"].values()) != metadata["land_pixels"]
                ):
                    raise ValueError("Land counts differ in the domain inventories")
                return (
                    tiles,
                    set(inventory["tiles"]),
                    [*files, domain / "modis_tiles.json"],
                )
        for rr in range(w["row"] // 256 * 256, bottom, 256):
            for cc in range(w["col"] // 256 * 256, right, 256):
                row, col = max(rr, w["row"]), max(cc, w["col"])
                tile = dict(
                    row=row,
                    col=col,
                    height=min(rr + 256, bottom) - row,
                    width=min(cc + 256, right) - col,
                )
                mask = land_mask(tile, rasters, bool(domain))
                count = int(mask.sum())
                if count:
                    tiles.append(dict(**tile, land_pixels=count))
                    vi_tiles.update(
                        np.unique(aqua_tiles(land_points(tile, mask, grid)))
                    )
    if not tiles:
        raise ValueError("Requested window contains no land pixels")
    return tiles, vi_tiles, files


def point_block(config, tile):
    row, col = tile["row"] // 256 * 256, tile["col"] // 256 * 256
    return config.sources["point_blocks"] / f"r{row}_c{col}_b0/points.parquet"


def timezones(config, points):
    if points.empty:
        return np.array([], dtype=object)
    table = config.sources.get("timezones")
    if table is not None:
        frame = (
            pd.read_parquet(table) if table.suffix == ".parquet" else pd.read_csv(table)
        )
        if frame["index"].duplicated().any():
            raise ValueError("Duplicate timezone point IDs")
        indexed = frame.set_index("index")
        missing = set(points["index"]) - set(indexed.index)
        if missing:
            raise ValueError(f"Missing timezone table rows: {len(missing)}")
        selected = indexed.loc[points["index"]]
        np.testing.assert_allclose(
            selected[["lat", "lon"]], points[["lat", "lon"]], rtol=0, atol=1e-10
        )
        zones = selected.tz.to_numpy()
    else:
        key = digest(points.to_dict(orient="list"))
        input_path = config.destination(f"tmp/timezones_{key}.input.csv")
        output_path = config.destination(f"tmp/timezones_{key}.csv")
        input_path.parent.mkdir(parents=True, exist_ok=True)
        points.to_csv(input_path, index=False)
        subprocess.run(
            [
                "Rscript",
                "--vanilla",
                str(Path(__file__).with_name("timezones.R")),
                str(input_path),
                str(output_path),
            ],
            check=True,
            capture_output=True,
            text=True,
        )
        frame = pd.read_csv(output_path)
        np.testing.assert_array_equal(frame["index"], points["index"])
        zones = frame.tz.to_numpy()
    for zone in pd.unique(zones):
        if pd.notna(zone):
            ZoneInfo(zone)
    return zones


def aqua_tiles(points):
    projection = "+proj=sinu +R=6371007.181 +nadgrids=@null +wktext"
    x, y = transform("EPSG:4326", projection, points.lon.tolist(), points.lat.tolist())
    span = 1111950.5196666666
    horizontal = np.floor((np.array(x) + 20015109.354) / span).astype(int)
    vertical = np.floor((10007554.677 - np.array(y)) / span).astype(int)
    return np.array([f"h{h:02d}v{v:02d}" for h, v in zip(horizontal, vertical)])
