"""Proxy predictions and monthly train-side spatial correction."""

import numpy as np
import pandas as pd
import torch
from sklearn.neighbors import BallTree
from pecan_downscaling.contracts import NUMERIC, predictors
from pecan_downscaling.artifacts import read
from pecan_downscaling.models.bundle import validate
from pecan_downscaling.models.network import ProxyNetwork
from pathlib import Path


def average_finite(a, axis=0):
    ok = np.isfinite(a)
    n = ok.sum(axis=axis)
    s = np.where(ok, a, 0).sum(axis=axis)
    return np.divide(s, n, out=np.full_like(s, np.nan, dtype=float), where=n > 0)


class Spatial:
    def __init__(self, path):
        with np.load(path) as z:
            self.z = {k: z[k] for k in z.files}
        self.ids = {}
        self.trees = {}
        self.base = {}
        global_ = average_finite(self.z["cell"])
        for code in range(1, 9):
            ids = np.flatnonzero(self.z["lc"] == code)
            self.ids[code] = ids
            local = average_finite(self.z["cell"][ids])
            self.base[code] = np.where(np.isfinite(local), local, global_)
            if len(ids):
                self.trees[code] = BallTree(
                    np.deg2rad(self.z["coords"][ids]), metric="haversine"
                )

    def query(self, points, lc):
        out = np.full((12, len(points), 2), np.nan)
        for code in np.unique(lc):
            take = np.flatnonzero(lc == code)
            ids = self.ids[code]
            for start in range(0, len(take), 512):
                ix = take[start : start + 512]
                if len(ids):
                    _, near = self.trees[code].query(
                        np.deg2rad(points[ix]), k=min(20, len(ids))
                    )
                    value = average_finite(self.z["cell"][ids[near]], axis=1)
                    out[:, ix] = np.where(
                        np.isfinite(value), value, self.base[code]
                    ).transpose(1, 0, 2)
                else:
                    out[:, ix] = self.base[code][:, None]
        return np.clip(out, -self.z["caps"], self.z["caps"])


class Models:
    def __init__(self, root, device="cpu", cpu_threads=1):
        self.root = Path(root)
        self.manifest = validate(root)
        self.device = device
        if device not in ("cpu", "cuda"):
            raise ValueError("Choose cpu or cuda")
        if device == "cuda" and not torch.cuda.is_available():
            raise RuntimeError("CUDA requested but no CUDA device is available")
        if device == "cuda":
            import cupy as cp

            if cp.cuda.runtime.getDeviceCount() != 1:
                raise RuntimeError("Exactly one visible CUDA device required")
        torch.set_num_threads(cpu_threads)
        self.proxies = {}
        self.metadata = {}

        self.spatial = {}
        for fold in (1, 2, 3):
            metadata = read(self.root / f"proxy/fold_{fold}.json")
            if metadata["preprocessing"]["numeric_names"] != NUMERIC or metadata[
                "preprocessing"
            ]["lc_levels"] != list(range(12)):
                raise ValueError("Proxy feature/category order mismatch")
            candidate = metadata["candidate"]
            model = ProxyNetwork(
                len(NUMERIC),
                int(candidate["width"]),
                int(candidate["blocks"]),
                candidate["sharing"],
                use_year_embedding=bool(candidate["use_year_embedding"]),
            )
            state = torch.load(
                self.root / f"proxy/fold_{fold}.pt",
                map_location="cpu",
                weights_only=True,
            )
            model.load_state_dict(state, strict=True)
            model.to(device)
            model.eval()
            self.proxies[fold] = model
            self.metadata[fold] = metadata
            self.spatial[fold] = Spatial(self.root / f"spatial/fold_{fold}.npz")

    def proxy(self, frame):
        raw, lc, valid = predictors(frame)
        output = np.full((3, len(frame), 2), np.nan, np.float32)
        if not valid.any():
            return output, valid
        years = pd.to_datetime(frame.period_key).dt.year.to_numpy()
        output[:, valid] = self.proxy_arrays(raw[valid], lc[valid], years[valid])
        return output, valid

    def proxy_arrays(self, raw, lc, years):
        output = np.empty((3, len(raw), 2), np.float32)
        for fold in (1, 2, 3):
            metadata = self.metadata[fold]
            pre = metadata["preprocessing"]
            numeric = (
                (raw - np.asarray(pre["numeric_mean"])) / np.asarray(pre["numeric_std"])
            ).astype(np.float32)
            categories = lc.astype(np.int64)
            year_ids = np.array(
                [
                    int(y) - 2012 if y in pre["fit_years"] and 2013 <= y <= 2024 else 0
                    for y in years
                ],
                np.int64,
            )
            pieces = []
            with torch.inference_mode():
                for begin in range(0, len(numeric), 8192):
                    end = begin + 8192
                    prediction = self.proxies[fold](
                        torch.as_tensor(numeric[begin:end], device=self.device),
                        torch.as_tensor(categories[begin:end], device=self.device),
                        torch.as_tensor(year_ids[begin:end], device=self.device),
                    )
                    pieces.append(
                        prediction[:, :, 0].cpu().numpy()
                        * np.asarray(metadata["target_scale"])
                        + np.asarray(metadata["target_center"])
                    )
            output[fold - 1] = np.concatenate(pieces)
        if not np.isfinite(output).all():
            raise ValueError("Nonfinite eligible proxy prediction")
        return output
