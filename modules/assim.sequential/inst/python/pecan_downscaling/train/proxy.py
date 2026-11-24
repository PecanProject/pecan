"""Proxy epoch selection, fit-side refitting and nested out-of-fold predictions."""

import gc
import time
import numpy as np
import torch
from pecan_downscaling.artifacts import receipt, require
from pecan_downscaling.contracts import NUMERIC
from pecan_downscaling.models.network import ProxyNetwork
from pecan_downscaling.train.recipe import CANDIDATE, SETTINGS, SEED, JITTER_SIGMA
from pecan_downscaling.train.optimization import prepare, configure_torch, fit_model
from pecan_downscaling.train.splits import sites, assert_split


def name(outer, inner=0, nested=0):
    return f"proxy_o{outer}_j{inner}_k{nested}"


def outputs(outer, inner=0, nested=0):
    key = name(outer, inner, nested)
    return [f"models/{key}.pt", f"predictions/{key}.npz", f"validation/{key}.json"]


def build(device):
    model = ProxyNetwork(
        len(NUMERIC),
        CANDIDATE["width"],
        CANDIDATE["blocks"],
        CANDIDATE["sharing"],
        use_year_embedding=CANDIDATE["use_year_embedding"],
        lc_categories=9,
    )
    # Preserve the original initialization/RNG sequence: the training wrapper
    # replaced a nine-category embedding AFTER constructing the entire network.
    # Initialize with that sequence while sharing the deployed network definition.
    model.backbone.lc_embedding = torch.nn.Embedding(12, 4)
    return model.to(device)


def training_rows(data, outer, inner=0, nested=0):
    fit_sites, held_sites = sites(data.folds, outer, inner, nested)
    row_sites = np.tile(data.arrays["site_ids"], data.p)
    available_rows = np.flatnonzero(data.arrays["complete"])
    fit_all = available_rows[np.isin(row_sites[available_rows], fit_sites["index"])]
    test = available_rows[np.isin(row_sites[available_rows], held_sites["index"])]
    active = fit_sites[fit_sites["index"].isin(set(row_sites[fit_all]))]
    rng = np.random.default_rng(
        5000 + (nested if nested else outer) * 10 + (0 if nested else inner)
    )
    validation_sites = []
    for _, group in active.groupby("LC", sort=True):
        ids = rng.permutation(group["index"])
        count = min(len(ids) - 1, max(1, round(len(ids) * 0.2)))
        validation_sites.extend(ids[: max(count, 0)])
    if not validation_sites:
        ids = rng.permutation(active["index"])
        validation_sites = list(ids[: max(1, len(ids) // 5)])
    fit = fit_all[~np.isin(row_sites[fit_all], validation_sites)]
    validation = fit_all[np.isin(row_sites[fit_all], validation_sites)]
    if not len(fit) or not len(validation) or not len(test):
        raise ValueError(f"Insufficient active sites for {name(outer, inner, nested)}")
    assert_split(
        np.unique(row_sites[fit]), np.unique(row_sites[validation]), held_sites["index"]
    )
    return fit_all, fit, validation, test, validation_sites, fit_sites, held_sites


def infer(model, prepared, rows):
    """Return float predictions (sample, NEE/LE, location/scale) in physical units."""
    model.eval()
    device = next(model.parameters()).device
    result = []
    with torch.inference_mode():
        for start in range(0, len(rows), 8192):
            selected = rows[start : start + 8192]
            output = model(
                torch.as_tensor(prepared.numeric[selected], device=device),
                torch.as_tensor(prepared.lc[selected], device=device),
                torch.as_tensor(prepared.year_ids[selected], device=device),
            )
            location = (
                output[:, :, 0].cpu().numpy() * prepared.target_scale
                + prepared.target_center
            )
            scale = (
                float(SETTINGS["gaussian_scale_floor"])
                + torch.nn.functional.softplus(output[:, :, 1])
            ).cpu().numpy() * prepared.target_scale
            result.append(np.stack([location, scale], axis=-1))
    value = np.concatenate(result)
    if not np.isfinite(value).all() or not (value[:, :, 1] > 0).all():
        raise ValueError("Invalid proxy prediction")
    return value


def load_model(run, key):
    path = run.root / f"models/{key}.pt"
    require(path, run.token)
    checkpoint = torch.load(path, map_location="cpu", weights_only=True)
    if (
        checkpoint["token"] != run.token
        or checkpoint["dataset"] != run.data.manifest["id"]
    ):
        raise ValueError("Proxy checkpoint belongs to a different training run")
    model = build(run.config.device)
    model.load_state_dict(checkpoint["state_dict"], strict=True)
    return model.eval(), checkpoint


def train(run, outer, inner=0, nested=0):
    key = name(outer, inner, nested)
    start = time.perf_counter()
    device = run.config.device
    torch.set_num_threads(run.config.cpu_threads)
    if device == "cuda":
        if not torch.cuda.is_available() or torch.cuda.device_count() != 1:
            raise RuntimeError("Training requires exactly one visible CUDA device")
        torch.cuda.reset_peak_memory_stats()
    fit_all, fit, validation, test, validation_sites, fit_sites, held_sites = (
        training_rows(run.data, outer, inner, nested)
    )
    configure_torch(SEED, device)
    prepared = prepare(run.data.arrays, fit)
    model = build(device)
    epoch, history = fit_model(
        model,
        prepared,
        fit,
        validation,
        SETTINGS,
        device,
        SEED,
        JITTER_SIGMA,
        SETTINGS["max_epochs"],
    )
    del model, prepared
    gc.collect()
    if device == "cuda":
        torch.cuda.empty_cache()
    configure_torch(SEED, device)
    prepared = prepare(run.data.arrays, fit_all)
    model = build(device)
    _, refit = fit_model(
        model, prepared, fit_all, None, SETTINGS, device, SEED, JITTER_SIGMA, epoch
    )
    prediction = infer(model, prepared, test)
    probe = test[:128]
    # Match batch shape across reloads so GPU kernel selection is identical.
    original = infer(model, prepared, probe)
    metadata = dict(
        candidate=CANDIDATE,
        settings=SETTINGS,
        preprocessing=prepared.preprocessing,
        target_center=prepared.target_center.tolist(),
        target_scale=prepared.target_scale.tolist(),
    )
    checkpoint = dict(
        token=run.token,
        dataset=run.data.manifest["id"],
        state_dict=model.state_dict(),
        metadata=metadata,
        train_sites=fit_sites["index"].tolist(),
        test_sites=held_sites["index"].tolist(),
        outer=outer,
        inner=inner,
        nested=nested,
    )
    path = run.path(f"models/{key}.pt")
    temporary = path.with_name(path.name + ".partial")
    torch.save(checkpoint, temporary)
    temporary.replace(path)
    receipt(path, run.token)
    restored, _ = load_model(run, key)
    np.testing.assert_allclose(
        original, infer(restored, prepared, probe), rtol=0, atol=1e-6
    )
    run.save(
        f"predictions/{key}.npz",
        rows=test,
        location=prediction[:, :, 0].astype(np.float32),
        scale=prediction[:, :, 1].astype(np.float32),
    )
    run.json(
        f"validation/{key}.json",
        dict(
            status="passed",
            best_epoch=epoch,
            history=history,
            refit_history=refit,
            total_seconds=time.perf_counter() - start,
            train_rows=len(fit_all),
            prediction_rows=len(test),
            train_sites=fit_sites["index"].tolist(),
            test_sites=held_sites["index"].tolist(),
            validation_sites=validation_sites,
            checkpoint_reload=True,
            cuda_peak_bytes=(
                torch.cuda.max_memory_allocated() if device == "cuda" else 0
            ),
        ),
    )


def assembled_path(outer, inner=0):
    return f"cache/proxy_o{outer}_j{inner}.npy"


def assemble(run, outer, inner=0):
    data = run.data
    result = np.full((data.rows, 2, 2), np.nan, np.float32)
    seen = np.zeros(data.rows, bool)
    keys = (
        [(outer, inner, nested) for nested in (1, 2, 3)]
        if inner
        else [(outer, held, 0) for held in (0, 1, 2, 3)]
    )
    for key in keys:
        values = run.load(f"predictions/{name(*key)}.npz")
        rows = values["rows"]
        if (
            np.any(rows < 0)
            or np.any(rows >= data.rows)
            or len(np.unique(rows)) != len(rows)
            or seen[rows].any()
        ):
            raise ValueError("Overlapping or invalid proxy out-of-fold rows")
        result[rows, :, 0], result[rows, :, 1] = values["location"], values["scale"]
        seen[rows] = True
    mask = (
        np.ones(data.n, bool)
        if not inner
        else (
            (data.folds.fold.to_numpy() != outer)
            & (data.folds[f"inner_{outer}"].to_numpy() != inner)
        )
    )
    np.testing.assert_array_equal(seen, data.arrays["complete"] & np.tile(mask, data.p))
    if not np.isfinite(result[seen]).all() or not (result[seen, :, 1] > 0).all():
        raise ValueError("Incomplete proxy out-of-fold values")
    run.array_save(assembled_path(outer, inner), result)
