# debias.py
import numpy as np
from sklearn.model_selection import GridSearchCV
from sklearn.neighbors import KNeighborsRegressor
from sklearn.ensemble import ExtraTreesRegressor
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import StandardScaler
from sklearn.metrics import mean_squared_error

_models = {}  # name -> dict(knn, tree, w, feat_names, feat_importances)

def _fit_knn(X, y):
    pipe = make_pipeline(StandardScaler(), KNeighborsRegressor())
    grid = GridSearchCV(
        pipe,
        {'kneighborsregressor__n_neighbors': list(range(1, 31))},
        cv=max(2, min(5, len(y))),
        scoring='neg_root_mean_squared_error',
        n_jobs=-1
    )
    grid.fit(X, y)
    k = grid.best_params_['kneighborsregressor__n_neighbors']
    knn = make_pipeline(StandardScaler(), KNeighborsRegressor(n_neighbors=k))
    knn.fit(X, y)
    return knn

def _fit_extratrees(X, y):
    param_grid = {
        'n_estimators': [200, 400, 800],
        'max_depth': [None, 20, 40],
        'max_features': ['sqrt', 'log2', None],
        'min_samples_leaf': [1, 2, 4],
        'min_samples_split': [2, 5, 10],
    }
    base = ExtraTreesRegressor(random_state=42, n_jobs=-1)
    grid = GridSearchCV(
        base, param_grid,
        cv=max(2, min(5, len(y))),
        scoring='neg_root_mean_squared_error',
        n_jobs=-1
    )
    grid.fit(X, y)
    tree = grid.best_estimator_
    tree.fit(X, y)
    return tree

def _fit_one(X, y):
    knn = _fit_knn(X, y)
    tree = _fit_extratrees(X, y)
    # simple blend weight by training RMSE grid (kept for speed)
    knn_pred  = knn.predict(X)
    tree_pred = tree.predict(X)
    weights = np.linspace(0, 1, 101)
    best_w, best_rmse = 0.5, np.inf
    for w in weights:
        rmse = np.sqrt(mean_squared_error(y, w*knn_pred + (1-w)*tree_pred))
        if rmse < best_rmse:
            best_rmse, best_w = rmse, w
    return knn, tree, float(best_w)

def train_full_model(name, X, y, feature_names=None):
    name = str(name)
    X = np.asarray(X); y = np.asarray(y)
    knn, tree, w = _fit_one(X, y)

    # stash explainability from the tree
    if feature_names is None:
        feature_names = [f"X{j}" for j in range(X.shape[1])]
    feat_importances = getattr(tree, "feature_importances_", None)
    if feat_importances is None:
        feat_importances = np.zeros(X.shape[1], dtype=float)

    _models[name] = {
        "knn": knn,
        "tree": tree,
        "w": float(w),
        "feat_names": list(map(str, feature_names)),
        "feat_importances": np.asarray(feat_importances, dtype=float).tolist()
    }
    
    return {
        "names": list(map(str, feature_names)),
        "importances": np.asarray(feat_importances, dtype=float).tolist()
    }

def has_model(name):
    return str(name) in _models

def get_model_weights(name):
    m = _models.get(str(name))
    return None if m is None else float(m["w"])

def get_feature_importance(name):
    m = _models.get(str(name))
    if m is None:
        return None
    return {
        "names": m["feat_names"],
        "importances": m["feat_importances"]
    }

def predict_residual(name, X):
    name = str(name)
    X = np.asarray(X)
    m = _models.get(name)
    if m is None:
        return np.zeros(X.shape[0])
    knn, tree, w = m["knn"], m["tree"], m["w"]
    return w * knn.predict(X) + (1 - w) * tree.predict(X)
