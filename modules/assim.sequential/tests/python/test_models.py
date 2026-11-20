"""Fast bundle integrity checks and optional acquired-model CPU/CUDA parity."""

import pytest
from pecan_downscaling.artifacts import write, sha, digest
from pecan_downscaling.models.bundle import SCHEMA, feature_contract, required_files, validate


def test_bundle_integrity(tmp_path):
    files = {}
    for name in required_files():
        path = tmp_path / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_bytes(b"model")
        files[name] = sha(path)
    manifest = dict(schema=SCHEMA, contract=feature_contract(), files=files)
    manifest["id"] = digest(manifest)
    write(tmp_path / "manifest.json", manifest)
    assert validate(tmp_path) == manifest
    path = tmp_path / next(iter(files))
    path.write_bytes(b"changed")
    with pytest.raises(ValueError, match="hash mismatch"):
        validate(tmp_path)
    path.unlink()
    with pytest.raises(ValueError, match="Missing model"):
        validate(tmp_path)
    manifest["contract"]["proxy_lc"] = [1]
    write(tmp_path / "manifest.json", manifest)
    with pytest.raises(ValueError, match="feature contract"):
        validate(tmp_path)
