"""Map forest splits to distinct inputs for this period."""

import copy, json
import numpy as np, pandas as pd
from pecan_downscaling.calendar import vi_calendar


def layout(row, root):
    models = {}
    used = set()
    times = pd.date_range(row.start, row.stop, freq="3h", inclusive="left")
    dates = vi_calendar(
        row.start - pd.Timedelta(days=16), row.stop + pd.Timedelta(days=16)
    )
    matched = [int(abs(dates - t.normalize()).argmin()) for t in times]
    for o in (1, 2, 3):
        for target in ("NEE", "LE"):
            path = root / f"residual/fold_{o}_{target}.json"
            d = json.loads(path.read_text())
            models[o, target] = d
            for t in d["learner"]["gradient_booster"]["model"]["trees"]:
                used.update(
                    int(f)
                    for f, left in zip(t["split_indices"], t["left_children"])
                    if left != -1
                )
    keys = {}
    representatives = []
    mapping = {}
    # XGBoost still needs an input column for a forest of constant leaves.
    for i in sorted(used or {4219}):
        if i < 3968:
            step, var = divmod(i, 31)
            if step >= len(times):
                key = ("nan",)
            elif var in (5, 13):
                key = ("coordinate", var)
            elif var in (10, 11, 12, 14, 15, 16, 17):
                key = ("annual", var, times[step].year)
            elif var in (8, 9, 22):
                key = ("vegetation", var, matched[step])
            else:
                key = ("native", step, var)
        elif i < 4096:
            key = ("constant", float(i - 3968 < len(times)))
        elif 4206 <= i < 4219:
            key = (
                "constant",
                float(i - 4206 == pd.Timestamp(row.period_key).year - 2012),
            )
        elif i == 4219:
            key = ("constant", len(times) / 128)
        else:
            key = ("context", i)
        if key not in keys:
            keys[key] = len(keys)
            representatives.append(i)
        mapping[i] = keys[key]
    return models, np.array(representatives), mapping


def compile_models(raw, representatives, mapping, device):
    import xgboost as xgb

    result = {}
    for key, source in raw.items():
        model = copy.deepcopy(source)
        model["learner"]["learner_model_param"]["num_feature"] = str(
            len(representatives)
        )
        for t in model["learner"]["gradient_booster"]["model"]["trees"]:
            t["tree_param"]["num_feature"] = str(len(representatives))
            t["split_indices"] = [
                mapping[int(i)] if left != -1 else 0
                for i, left in zip(t["split_indices"], t["left_children"])
            ]
        booster = xgb.Booster()
        booster.load_model(bytearray(json.dumps(model).encode()))
        booster.set_param(dict(device=device, nthread=1))
        result[key] = booster
    return result
