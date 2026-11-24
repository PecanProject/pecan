"""Explicit site IDs and immutable outer, inner and nested fold membership."""

import numpy as np
import pandas as pd


def align_ids(source, requested, label):
    source, requested = pd.Index(source), pd.Index(requested)
    if (
        source.hasnans
        or requested.hasnans
        or not source.is_unique
        or not requested.is_unique
    ):
        raise ValueError(f"Duplicate or missing identifiers: {label}")
    positions = source.get_indexer(requested)
    if (positions < 0).any():
        raise ValueError(
            f"Missing identifiers in {label}: {requested[positions < 0].tolist()[:10]}"
        )
    return positions


def split_assign(table, seed):
    rng = np.random.default_rng(seed)
    result = pd.Series(index=table.index, dtype="int64")
    for _, group in table.groupby(["LC", "is_tower"], sort=True):
        positions = rng.permutation(group.index.to_numpy())
        offset = int(rng.integers(3))
        result.loc[positions] = (np.arange(len(positions)) + offset) % 3 + 1
    return result.astype(int)


def declare_nested(table):
    """Call on the full site inventory BEFORE making a bounded subset."""
    table = table.copy()
    for outer in (1, 2, 3):
        for inner in (1, 2, 3):
            name = f"nested_{outer}_{inner}"
            selected = (table.fold != outer) & (table[f"inner_{outer}"] != inner)
            expected = split_assign(table[selected], 123 + 100 * outer + inner)
            if name in table:
                np.testing.assert_array_equal(table.loc[selected, name], expected)
            table[name] = 0
            table.loc[selected, name] = expected
    return table


def assert_split(fit, held, excluded=()):
    if (
        not len(fit)
        or not len(held)
        or set(fit) & set(held)
        or set(fit) & set(excluded)
    ):
        raise ValueError("Invalid or leaking site split")


def sites(table, outer, inner=0, nested=0):
    if (
        outer not in (1, 2, 3)
        or inner not in (0, 1, 2, 3)
        or nested not in (0, 1, 2, 3)
        or (nested and not inner)
    ):
        raise ValueError("Unknown fold request")
    outer_train = table.fold != outer
    if nested:
        candidates = outer_train & (table[f"inner_{outer}"] != inner)
        fold = table[f"nested_{outer}_{inner}"]
        fit, held = candidates & (fold != nested), candidates & (fold == nested)
    elif inner:
        fold = table[f"inner_{outer}"]
        fit, held = outer_train & (fold != inner), outer_train & (fold == inner)
    else:
        fit, held = outer_train, ~outer_train
    assert_split(
        table.loc[fit, "index"],
        table.loc[held, "index"],
        table.loc[~outer_train, "index"],
    )
    return table[fit], table[held]


def proxy_keys():
    return [
        (outer, inner, nested)
        for outer in (1, 2, 3)
        for inner, nested in [(0, 0), (1, 0), (2, 0), (3, 0)]
        + [(inner, nested) for inner in (1, 2, 3) for nested in (1, 2, 3)]
    ]


def validate(table):
    align_ids(table["index"], table["index"], "training sites")
    if (
        not np.isin(table.fold, (1, 2, 3)).all()
        or not np.isin(table.LC, np.arange(1, 9)).all()
    ):
        raise ValueError("Unknown site fold or land cover")
    if table.is_tower.isna().any() or not table.is_tower.isin([True, False]).all():
        raise ValueError("Invalid tower membership")
    for outer in (1, 2, 3):
        fit = table.fold != outer
        membership = table[f"inner_{outer}"]
        if not (
            np.isin(membership[fit], (1, 2, 3)).all() and (membership[~fit] == 0).all()
        ):
            raise ValueError("Invalid inner membership")
        for inner in (1, 2, 3):
            selected = fit & (membership != inner)
            nested = table[f"nested_{outer}_{inner}"]
            if not (
                np.isin(nested[selected], (1, 2, 3)).all()
                and (nested[~selected] == 0).all()
            ):
                raise ValueError("Invalid nested membership")
    for key in proxy_keys():
        sites(table, *key)
