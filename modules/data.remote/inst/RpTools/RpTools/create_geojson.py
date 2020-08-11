#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
used to Create GeoJSON file from the geometry extracted from sites table in BETY

Author: Ayush Prasad
"""


from geojson import Point, Feature, FeatureCollection, dump
import json
import os


def create_geojson(coords, siteid, outdir):
    """
    Create GeoJSON file from geometry extracted from BETY

    Parameters
    ----------
    coords (str) -- geometry from BETY sites
    siteid (str) -- siteid
    outdir (str) -- path where the output file has to be stored
    Returns
    -------
    Absolute path to the merged file
    output GeoJSOn file is saved in the specified directory.
    """

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
