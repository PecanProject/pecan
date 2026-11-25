"""Compare two products/period directories for the same window on the same device."""

import argparse
from pathlib import Path
import numpy as np
import rasterio


def compare(reference, candidate):
    expected = sorted(p.name for p in reference.glob("*.tif"))
    if not expected or expected != sorted(p.name for p in candidate.glob("*.tif")):
        raise ValueError("Missing or differing period maps")
    for name in expected:
        with rasterio.open(reference / name) as ref, rasterio.open(candidate / name) as new:
            assert ref.count == new.count == 7
            assert (ref.crs, ref.transform, ref.shape, ref.descriptions, ref.dtypes) == (
                new.crs, new.transform, new.shape, new.descriptions, new.dtypes
            )
            assert np.isnan(ref.nodata) and np.isnan(new.nodata)
            for key in ("target", "period_id", "start_utc", "stop_utc_exclusive",
                        "units", "missing_reason_bits"):
                assert ref.tags()[key] == new.tags()[key], (name, key)
            eligible = 0
            for _, window in ref.block_windows(1):
                a, b = ref.read(window=window), new.read(window=window)
                np.testing.assert_array_equal(a, b, err_msg=name)
                np.testing.assert_array_equal(ref.read_masks(window=window),
                                              new.read_masks(window=window))
                eligible += np.count_nonzero(b[5] == 1)
            print(f"{name}: all seven bands and masks match; eligible pixels: {eligible}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("reference", type=Path)
    parser.add_argument("candidate", type=Path)
    args = parser.parse_args()
    compare(args.reference, args.candidate)
