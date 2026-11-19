"""Feature order, units and missingness contracts shared by all inference paths."""

import numpy as np
import pandas as pd
from rasterio.warp import transform

TARGETS = ["NEE", "LE"]

KGC = [
    "Af",
    "Am",
    "Aw",
    "BSh",
    "BSk",
    "BWh",
    "BWk",
    "Cfa",
    "Cfb",
    "Cfc",
    "Csa",
    "Csb",
    "Cwa",
    "Cwb",
    "Dfa",
    "Dfb",
    "Dfc",
    "Dsb",
    "Dsc",
    "Dwa",
    "Dwb",
    "Dwc",
    "ET",
]

NATIVE = [
    "t2m",
    "sp",
    "d2m",
    "tp",
    "strd",
    "lon",
    "sin_doy",
    "cos_doy",
    "NDVI",
    "EVI",
    "twi",
    "PH",
    "Sand",
    "lat",
    "agb",
    "SOC",
    "N",
    "year_since_disturb",
    "PPFD",
    "WindSpeed",
    "t2m2",
    "PPFD2",
    "EVI2",
    "VPD_t2m",
    "sincos",
    "sm_0_10",
    "sm_10_40",
    "sm_40_100",
    "sm_100_200",
    "VPD",
    "is_day",
]

SOIL = ["sm_0_10", "sm_10_40", "sm_40_100", "sm_100_200"]

DYNAMIC = [
    "temp_c",
    "precipitation_m",
    "shortwave_w_m2",
    "vpd_kpa",
    "rh_fraction",
    "ppfd_umol_m2_s",
    "wind_speed_m_s",
    "EVI",
    "NDVI",
    "gldas_rootzone_vwc",
]

STATIC = ["year_since_disturb", "agb", "PH", "N", "SOC", "Sand", "lat", "lon", "DEM"]

TIME = [
    "time_sin_k1",
    "time_cos_k1",
    "time_sin_k4",
    "time_cos_k4",
    "photoperiod_hours",
    "projected_x_m",
    "projected_y_m",
    "hydrological_year_continuous",
]

NUMERIC = STATIC + DYNAMIC + TIME + [f"{n}_L{k}" for k in range(1, 5) for n in DYNAMIC]

CEA = "+proj=cea +lat_ts=30 +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"


def unique(frame, keys):
    if frame[keys].isna().any().any() or frame.duplicated(keys).any():
        raise ValueError(f"Missing or duplicate keys: {keys}")


def rootzone(layers):
    a = np.asarray(layers, dtype=float)
    if a.shape[-1] != 4:
        raise ValueError("Four GLDAS layers required")
    # Sum, not nansum: an absent constituent does not mean dry soil.
    return a[..., :3].sum(axis=-1) / 1000.0


def ppfd(shortwave_j_m2):
    return np.asarray(shortwave_j_m2) / 10800.0 * 0.45 * 4.57


def predictors(frame):
    """Same numeric fields for fitting and prediction. No target argument or label reads."""
    f = frame.copy()
    dates = pd.to_datetime(f.period_key)
    doy = dates.dt.dayofyear.to_numpy()
    for k in [1, 4]:
        f[f"time_sin_k{k}"] = np.sin(2 * np.pi * k * doy / 365.25)
        f[f"time_cos_k{k}"] = np.cos(2 * np.pi * k * doy / 365.25)
    decl = 0.409 * np.sin(2 * np.pi * doy / 365 - 1.39)
    f["photoperiod_hours"] = (
        24
        / np.pi
        * np.arccos(np.clip(-np.tan(np.deg2rad(f.lat)) * np.tan(decl), -1, 1))
    )
    x, y = transform("EPSG:4326", CEA, f.lon.tolist(), f.lat.tolist())
    f["projected_x_m"] = x
    f["projected_y_m"] = y
    f["hydrological_year_continuous"] = dates.dt.year + (
        dates.dt.strftime("%m-%d") >= "07-16"
    ).astype(int)
    values = f[NUMERIC].to_numpy(np.float32)
    lc = f.LC_proxy.to_numpy(float)
    valid = (
        np.isfinite(values).all(axis=1)
        & np.isfinite(lc)
        & (lc >= 0)
        & (lc <= 11)
        & (lc == np.floor(lc))
    )
    return values, lc, valid


def add_previous(current, history):
    a = current.copy()
    for k in range(1, 5):
        prev = (
            history[int(a.period_id.iloc[0]) - k].set_index("index").reindex(a["index"])
        )
        for n in DYNAMIC:
            a[f"{n}_L{k}"] = prev[n].to_numpy()
    return a


def validate_lc(values, name):
    values = np.asarray(values)
    unknown = set(values[np.isfinite(values)]) - set(range(12))
    if unknown:
        raise ValueError(f"Unknown {name} encoding: {unknown}")
