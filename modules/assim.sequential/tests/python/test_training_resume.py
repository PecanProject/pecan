import numpy as np
import pytest

from pecan_downscaling.train.configuration import Config
from pecan_downscaling.train.engine import training_directory
from pecan_downscaling.train.state import Run


def test_stage_retry_and_verified_reuse(training_data, tmp_path):
    c = Config(training_data, tmp_path / "run")
    with training_directory(c.output, False):
        run = Run(c)
        run.freeze()
        outputs = ["cache/result.npy"]

        def write_result():
            run.array_save(outputs[0], np.arange(3))

        def interrupt():
            write_result()
            raise RuntimeError("interrupted")

        with pytest.raises(RuntimeError, match="interrupted"):
            run.stage("fixture", outputs, interrupt)
        assert run.stage("fixture", outputs, write_result)
    with training_directory(c.output, True):
        run = Run(c)
        run.freeze()
        assert not run.stage("fixture", outputs, lambda: pytest.fail("repeated stage"))
        (run.root / outputs[0]).rename(run.root / "missing.npy")
        with pytest.raises(ValueError, match="Missing"):
            run.stage("fixture", outputs, write_result)
        with pytest.raises(ValueError, match="changed"):
            Run(Config(training_data, c.output, cpu_threads=2)).freeze()
        with pytest.raises(RuntimeError, match="Another process"):
            with training_directory(c.output, True):
                pytest.fail("concurrent access")
    with pytest.raises(ValueError, match="resume"):
        with training_directory(c.output, False):
            pytest.fail("overwrote training")
