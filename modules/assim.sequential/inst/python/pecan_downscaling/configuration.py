"""Explicit inputs and a contained per-run output root; importing never writes."""

from dataclasses import dataclass
from pathlib import Path
import os
import yaml

REQUIRED_SOURCES = (
    "grid",
    "periods",
    "era",
    "soil_cache",
    "annual_covariates",
    "native_covariates",
    "native_lc",
    "kgc",
    "kgc_legend",
    "aqua_ledger",
)


@dataclass(frozen=True)
class Config:
    model_bundle: Path
    output: Path
    sources: dict
    window: dict
    period_ids: tuple
    device: str
    workers: int = 3
    cpu_threads: int = 1
    max_output_bytes: int = 50 * 1024**3
    panel_cache: Path | None = None

    def destination(self, relative):
        path = (self.output / relative).resolve()
        if not path.is_relative_to(self.output):
            raise ValueError(f"Output escapes run root: {relative}")
        return path


def load(path):
    path = Path(path).resolve()
    document = yaml.safe_load(path.read_text())
    if not isinstance(document, dict):
        raise ValueError("Configuration must be a mapping")
    unknown = set(document) - {
        "panel_cache",
        "model_bundle",
        "output",
        "sources",
        "request",
        "resources",
    }
    if unknown:
        raise ValueError(f"Unknown configuration fields: {sorted(unknown)}")

    def resolve(value):
        if not isinstance(value, str) or not value.strip():
            raise ValueError("Paths must be nonempty strings")
        expanded = Path(os.path.expandvars(value)).expanduser()
        return (
            (path.parent / expanded).resolve()
            if not expanded.is_absolute()
            else expanded.resolve()
        )

    try:
        bundle, output = resolve(document["model_bundle"]), resolve(document["output"])
        raw_sources = document["sources"]
        missing = set(REQUIRED_SOURCES) - set(raw_sources)
        if missing:
            raise ValueError(f"Missing sources: {sorted(missing)}")
        unknown_sources = (
            set(raw_sources)
            - set(REQUIRED_SOURCES)
            - {"timezones", "point_blocks", "spatial_cache", "domain"}
        )
        if unknown_sources:
            raise ValueError(f"Unknown sources: {sorted(unknown_sources)}")
        sources = {key: resolve(value) for key, value in raw_sources.items()}
        request = document["request"]
        window = request["window"]
        if set(window) != {"row", "col", "height", "width"}:
            raise ValueError("Window requires row, col, height and width")
        if (
            any(type(v) is not int for v in window.values())
            or window["row"] < 0
            or window["col"] < 0
            or min(window["height"], window["width"]) <= 0
        ):
            raise ValueError(
                "Window offsets must be nonnegative integers; dimensions must be positive"
            )
        periods = request["period_ids"]
        if (
            not isinstance(periods, list)
            or not periods
            or any(type(i) is not int or i < 1 for i in periods)
            or len(periods) != len(set(periods))
        ):
            raise ValueError("period_ids must be distinct positive integers")
        if set(request) != {"window", "period_ids"}:
            raise ValueError("Unknown request field")
        resources = document.get("resources", {})
        if set(resources) - {"device", "workers", "cpu_threads", "max_output_bytes"}:
            raise ValueError("Unknown resource field")
        device = resources.get("device", "cpu")
        if device not in ("cpu", "cuda"):
            raise ValueError("device must be cpu or cuda")
        numbers = [
            resources.get("workers", 3),
            resources.get("cpu_threads", 1),
            resources.get("max_output_bytes", 50 * 1024**3),
        ]
        if any(type(v) is not int or v < 1 for v in numbers):
            raise ValueError("Resource limits must be positive integers")
    except (KeyError, TypeError) as error:
        raise ValueError(f"Incomplete configuration: {error}") from error
    for source in [bundle, *sources.values()]:
        if output == source or output in source.parents or source in output.parents:
            raise ValueError(f"Input and output roots overlap: {source}")
    cache = resolve(document["panel_cache"]) if "panel_cache" in document else None
    if cache:
        for other in (output, bundle, *sources.values()):
            if cache.is_relative_to(other) or other.is_relative_to(cache):
                raise ValueError("Choose a separate writable panel_cache")
    return Config(
        bundle,
        output,
        sources,
        dict(window),
        tuple(sorted(periods)),
        device,
        *numbers,
        cache,
    )
