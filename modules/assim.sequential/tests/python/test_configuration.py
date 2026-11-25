import subprocess
import sys
from pathlib import Path
import pytest
import yaml
from pecan_downscaling.configuration import load


def config_file(tmp_path):
    template = Path(__file__).parents[2] / "inst/flux-maps/predict.yaml"
    document = yaml.safe_load(template.read_text())
    document["output"] = str(tmp_path / "run")
    document["model_bundle"] = str(tmp_path / "bundle")
    document["sources"] = {
        key: str(tmp_path / "inputs" / key) for key in document["sources"]
    }
    path = tmp_path / "run.yaml"
    path.write_text(yaml.safe_dump(document))
    return path, document


def test_config_is_read_only_and_paths_are_contained(tmp_path):
    path, _ = config_file(tmp_path)
    config = load(path)
    assert config.period_ids == (139,) and not config.output.exists()
    with pytest.raises(ValueError, match="escapes"):
        config.destination("../escape")
    result = subprocess.run(
        [sys.executable, "-c", "import pecan_downscaling; import pecan_downscaling.predict"],
        cwd=Path(__file__).parents[2] / "inst/python",
        capture_output=True,
        text=True,
    )
    assert result.returncode == 0, result.stderr
    assert not config.output.exists()


@pytest.mark.parametrize("change", ["overlap", "period", "device", "window", "unknown"])
def test_invalid_configuration(tmp_path, change):
    path, d = config_file(tmp_path)
    if change == "overlap":
        d["output"] = d["model_bundle"]
    elif change == "period":
        d["request"]["period_ids"] = [1, 1]
    elif change == "device":
        d["resources"]["device"] = "auto"
    elif change == "window":
        d["request"]["window"]["row"] = -1
    else:
        d["scientific_override"] = {"feature_order": []}
    path.write_text(yaml.safe_dump(d))
    with pytest.raises(ValueError):
        load(path)
