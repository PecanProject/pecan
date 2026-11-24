"""Train-side scaling and the fixed Student-t proxy optimization loop."""

from dataclasses import dataclass
import math
import os
import numpy as np
import torch
from pecan_downscaling.contracts import NUMERIC
from pecan_downscaling.train.recipe import JITTER_EXCLUDE


@dataclass
class Prepared:
    numeric: np.ndarray
    lc: np.ndarray
    year_ids: np.ndarray
    target_mean: np.ndarray
    target_center: np.ndarray
    target_scale: np.ndarray
    jitter_mask: np.ndarray
    preprocessing: dict


def prepare(arrays, fit_rows):
    """Fit statistics on selected rows only; preserve float64 reductions."""
    fit = arrays["numeric"][fit_rows]
    observed = np.isfinite(fit).sum(axis=0)
    if np.any(observed == 0):
        raise ValueError("Numeric features are entirely missing in fit rows")
    mean = np.nanmean(fit, axis=0, dtype=np.float64)
    std = np.nanstd(fit, axis=0, dtype=np.float64)
    std[std < 1e-8] = 1.0
    numeric = np.where(np.isfinite(arrays["numeric"]), arrays["numeric"], mean)
    numeric = ((numeric - mean) / std).astype(np.float32)
    center = arrays["target_mean"][fit_rows].mean(axis=0, dtype=np.float64)
    scale = arrays["target_mean"][fit_rows].std(axis=0, dtype=np.float64)
    scale[scale < 1e-8] = 1.0
    target = ((arrays["target_mean"] - center) / scale).astype(np.float32)
    fit_years = set(map(int, arrays["years"][fit_rows]))
    year_ids = np.asarray(
        [
            int(year) - 2012 if int(year) in fit_years and 2013 <= year <= 2024 else 0
            for year in arrays["years"]
        ],
        dtype=np.int64,
    )
    if not np.isin(arrays["lc"], np.arange(12)).all():
        raise ValueError("Unknown proxy land cover")
    metadata = dict(
        numeric_names=NUMERIC,
        numeric_mean=mean.tolist(),
        numeric_std=std.tolist(),
        numeric_fit_imputed_count=(len(fit_rows) - observed).tolist(),
        fit_years=sorted(fit_years),
        basis=dict(basis="none"),
        lc_levels=list(range(12)),
        jitter_exclude=JITTER_EXCLUDE,
    )
    return Prepared(
        numeric,
        np.asarray(arrays["lc"], np.int64),
        year_ids,
        target,
        center,
        scale,
        np.asarray([name not in JITTER_EXCLUDE for name in NUMERIC]),
        metadata,
    )


def configure_torch(seed, device):
    os.environ.setdefault("CUBLAS_WORKSPACE_CONFIG", ":4096:8")
    torch.manual_seed(seed)
    torch.use_deterministic_algorithms(True)
    if torch.device(device).type == "cuda":
        torch.cuda.manual_seed_all(seed)
        torch.backends.cudnn.benchmark = False
        torch.backends.cudnn.deterministic = True


def batch_tensors(prepared, rows, device, jitter_sigma, year_dropout, training):
    numeric = torch.from_numpy(prepared.numeric[rows]).to(device, non_blocking=True)
    lc = torch.from_numpy(prepared.lc[rows]).to(device, non_blocking=True)
    year = torch.from_numpy(prepared.year_ids[rows]).to(device, non_blocking=True)
    if training and jitter_sigma > 0:
        mask = torch.from_numpy(prepared.jitter_mask).to(device)
        numeric = numeric + torch.randn_like(numeric) * jitter_sigma * mask
    if training and year_dropout > 0:
        year = year.clone()
        year[torch.rand(len(year), device=device) < year_dropout] = 0
    target = torch.from_numpy(prepared.target_mean[rows]).to(device, non_blocking=True)
    return numeric, lc, year, target


def model_loss(output, target, settings):
    location = output[:, :, 0]
    scale = float(settings["gaussian_scale_floor"]) + torch.nn.functional.softplus(
        output[:, :, 1]
    )
    degrees = float(settings["student_df"])
    residual2 = ((target - location) / scale).square()
    constant = (
        torch.lgamma(torch.tensor((degrees + 1) / 2, device=output.device))
        - torch.lgamma(torch.tensor(degrees / 2, device=output.device))
        - 0.5 * math.log(degrees * math.pi)
    )
    log_probability = (
        constant
        - torch.log(scale)
        - ((degrees + 1) / 2) * torch.log1p(residual2 / degrees)
    )
    return -log_probability.mean() + float(
        settings["probability_point_weight"]
    ) * torch.nn.functional.mse_loss(location, target)


def evaluate_loss(model, prepared, rows, batch_size, device, settings):
    model.eval()
    total = 0.0
    with torch.no_grad():
        for start in range(0, len(rows), batch_size):
            selected = rows[start : start + batch_size]
            numeric, lc, year, target = batch_tensors(
                prepared, selected, device, 0.0, 0.0, False
            )
            loss = model_loss(model(numeric, lc, year), target, settings)
            total += float(loss.cpu()) * len(selected)
    return total / len(rows)


def fit_model(
    model,
    prepared,
    train_rows,
    validation_rows,
    settings,
    device,
    seed,
    jitter_sigma,
    max_epochs,
):
    optimizer = torch.optim.AdamW(
        model.parameters(),
        lr=float(settings["learning_rate"]),
        weight_decay=float(settings["weight_decay"]),
    )
    warmup_epochs = max(1, round(max_epochs * float(settings["warmup_fraction"])))

    def learning_rate(epoch):
        # LambdaLR starts at zero; optimizer epoch one needs warmup step one.
        epoch = min(epoch + 1, max_epochs)
        if epoch <= warmup_epochs:
            return epoch / warmup_epochs
        progress = (epoch - warmup_epochs) / max(1, max_epochs - warmup_epochs)
        return 0.5 * (1.0 + math.cos(math.pi * progress))

    scheduler = torch.optim.lr_scheduler.LambdaLR(optimizer, learning_rate)
    rng = np.random.default_rng(seed)
    batch_size, patience = int(settings["batch_size"]), int(settings["patience"])
    best_epoch, best_loss, stale = max_epochs, float("inf"), 0
    history = []
    for epoch in range(1, max_epochs + 1):
        model.train()
        shuffled = rng.permutation(train_rows)
        total = 0.0
        for start in range(0, len(shuffled), batch_size):
            selected = shuffled[start : start + batch_size]
            numeric, lc, year, target = batch_tensors(
                prepared,
                selected,
                device,
                jitter_sigma,
                float(settings["year_embedding_dropout"]),
                True,
            )
            optimizer.zero_grad(set_to_none=True)
            loss = model_loss(model(numeric, lc, year), target, settings)
            if not torch.isfinite(loss):
                raise RuntimeError(f"Nonfinite Student-t loss at epoch {epoch}")
            loss.backward()
            torch.nn.utils.clip_grad_norm_(
                model.parameters(), float(settings["gradient_clip"])
            )
            optimizer.step()
            total += float(loss.detach().cpu()) * len(selected)
        row = dict(
            epoch=float(epoch),
            train_loss=total / len(shuffled),
            learning_rate=float(optimizer.param_groups[0]["lr"]),
        )
        scheduler.step()
        if validation_rows is not None:
            value = evaluate_loss(
                model, prepared, validation_rows, batch_size, device, settings
            )
            if not np.isfinite(value):
                raise RuntimeError("Nonfinite epoch-selection loss")
            row["inner_validation_loss"] = value
            if value < best_loss - 1e-6:
                best_loss, best_epoch, stale = value, epoch, 0
            else:
                stale += 1
            if stale >= patience:
                history.append(row)
                break
        else:
            best_epoch = epoch
        history.append(row)
    return best_epoch, history
