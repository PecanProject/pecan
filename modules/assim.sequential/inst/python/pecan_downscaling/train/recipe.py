"""The effective scientific recipe, independent of experiment configuration files."""

from copy import deepcopy

SEED = 42
JITTER_SIGMA = 0.01
JITTER_EXCLUDE = [
    "lat",
    "lon",
    "time_sin_k1",
    "time_cos_k1",
    "time_sin_k4",
    "time_cos_k4",
    "photoperiod_hours",
    "projected_x_m",
    "projected_y_m",
    "hydrological_year_continuous",
]
CANDIDATE = dict(
    width=256, blocks=4, sharing="shared", basis="none", use_year_embedding=False
)
SETTINGS = dict(
    max_epochs=80,
    patience=10,
    batch_size=8192,
    learning_rate=0.0003,
    weight_decay=0.0001,
    warmup_fraction=0.05,
    gradient_clip=1.0,
    year_embedding_dropout=0.0,
    probability_point_weight=0.1,
    gaussian_scale_floor=0.02,
    student_df=4.0,
    quantiles=[0.05, 0.25, 0.5, 0.75, 0.95],
)
BOOSTING = dict(
    tree_method="hist",
    eta=0.1,
    max_depth=6,
    subsample=0.8,
    colsample_bytree=0.8,
    min_child_weight=10,
    gamma=0.5,
    reg_lambda=3,
    reg_alpha=2,
    seed=30001,
    max_bin=256,
)
ROUNDS = 200


def contract():
    return deepcopy(
        dict(
            candidate=CANDIDATE,
            settings=SETTINGS,
            seed=SEED,
            jitter_sigma=JITTER_SIGMA,
            jitter_exclude=JITTER_EXCLUDE,
            boosting=BOOSTING,
            rounds=ROUNDS,
            residual_features=4220,
            inner_seed=123,
            epoch_selection_fraction=0.2,
            spatial_neighbors=20,
            clip_quantile=0.999,
            routes="complete native period support",
            aggregation_scale="once per period; monthly spatial correction",
        )
    )
