import numpy as np
import pytest
from pecan_downscaling.artifacts import write
from pecan_downscaling.predict.products import block, store
from pecan_downscaling.predict.state import plan, locked


def publish_blocks(case, excluded=False):
    cfg, manifest, _, wanted = case
    row = next(wanted.itertuples())
    tile = manifest["tiles"][0]
    ids = np.array([5, 6, 9, 10])
    from pandas import DataFrame

    store(
        cfg,
        row,
        tile,
        dict(
            land=ids,
            meta=dict(period_panels=[]),
            reason_ids=ids[:0] if excluded else ids,
            reason=np.array([]) if excluded else np.zeros(len(ids)),
            points=DataFrame(dict(index=ids)),
        ),
        None if excluded else np.ones((3, len(ids), 2), np.float32),
        manifest["token"],
    )
    return block(cfg, row, tile)


def test_lock_and_source_identity(run_case):
    cfg, manifest, *_ = run_case
    assert not cfg.output.exists()
    with locked(cfg):
        with pytest.raises(RuntimeError), locked(cfg):
            pass
    write(cfg.destination("manifest.json"), manifest)
    assert plan(cfg)[0] == manifest
    cfg.sources["aqua_ledger"].rename(cfg.output / "moved.json")
    with pytest.raises(ValueError, match="disappeared"):
        plan(cfg)


def test_shared_panel_survives_run_code_and_model_changes(
    run_case, tmp_path, monkeypatch
):
    from types import SimpleNamespace
    import pandas as pd
    from pecan_downscaling.contracts import DYNAMIC
    from pecan_downscaling.predict import panels, state

    cfg, contract, needed, _ = run_case
    points = pd.DataFrame(dict(index=[7], lat=[45.0], lon=[-80.0]))
    src = SimpleNamespace(paths=cfg.sources, vegetation=lambda *_: None)
    row = next(needed.iloc[-1:].itertuples())
    tokens = contract["panel_sources"]
    builds = []

    def build(points, vi, period, src, timing):
        builds.append(period.period_id)
        return [points.assign(**{k: 1.0 for k in DYNAMIC})]

    monkeypatch.setattr(panels, "current_panel", build)
    panels.load_panels(points, needed, row, src, "block", tmp_path, tokens)
    monkeypatch.setattr(state, "code_identity", lambda: {"models/inference.py": "changed"})
    monkeypatch.setattr(state, "validate", lambda _: dict(id="new model"))
    from pecan_downscaling.artifacts import read

    ledger = read(cfg.sources["aqua_ledger"])
    ledger["dates"].append("A2019009")
    write(cfg.sources["aqua_ledger"], ledger)
    updated, *_ = plan(cfg)
    assert updated["token"] != contract["token"]
    tokens = updated["panel_sources"]
    _, _, audit = panels.load_panels(
        points, needed, row, src, "block", tmp_path, tokens
    )
    assert len(builds) == 5 and all(item["reused"] for item in audit)

    (cfg.sources["era"] / "ERA5_2018.nc").write_bytes(b"replaced ERA data")
    updated, *_ = plan(cfg)
    tokens = updated["panel_sources"]
    with pytest.raises(ValueError, match="Changed period panel inputs"):
        panels.load_panels(points, needed, row, src, "block", tmp_path, tokens)


def test_resume_after_block_before_maps(run_case, monkeypatch):
    from pecan_downscaling.predict import engine

    cfg, contract, _, _ = run_case
    run_period = engine.run_period
    monkeypatch.setattr(engine, "Models", lambda *args: None)

    def interrupt(*args):
        publish_blocks(run_case)
        raise InterruptedError("interrupted after publishing the tile")

    monkeypatch.setattr(engine, "run_period", interrupt)
    with pytest.raises(InterruptedError):
        engine.execute("run", cfg)
    tile = block(cfg, next(run_case[3].itertuples()), contract["tiles"][0])
    published = tile.stat().st_mtime_ns
    assert not cfg.destination("COMPLETE.json").exists()
    monkeypatch.setattr(engine, "run_period", run_period)
    with pytest.raises(ValueError, match="resume"):
        engine.execute("run", cfg)
    result = engine.execute("run", cfg, resume=True)
    assert result == engine.execute("report", cfg)
    assert [row["variable"] for row in result["products"]] == ["NEE", "LE"]
    assert tile.stat().st_mtime_ns == published
