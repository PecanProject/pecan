import json
import os


def create_geojson(coords, siteid, outdir):

    geo = json.loads(coords)

    features = []

    features.append(Feature(geometry=geo, properties={"name": siteid}))

    feature_collection = FeatureCollection(features)

    if not os.path.exists(outdir):
        os.makedirs(outdir, exist_ok=True)

    file = os.path.join(outdir, siteid + ".geojson")

    with open(file, "w") as f:
        dump(feature_collection, f)

    return os.path.abspath(file)
