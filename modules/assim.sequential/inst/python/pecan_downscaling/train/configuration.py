"""Local training paths and compute resources."""

from dataclasses import dataclass
from pathlib import Path


@dataclass(frozen=True)
class Config:
    prepared_data: Path
    output: Path
    device: str = "cpu"
    cpu_threads: int = 1

    def __post_init__(self):
        if self.device not in ("cpu", "cuda"):
            raise ValueError("Training device must be cpu or cuda")
        if type(self.cpu_threads) is not int or self.cpu_threads < 1:
            raise ValueError("cpu_threads must be a positive integer")
