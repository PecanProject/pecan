#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Mon Feb 24 09:37:54 2020

@author:
    Olli Nevalainen (olli.nevalainen@fmi.fi), Finnish Meteorological Institute)
"""
import geopandas as gpd
import shapely
from zipfile import ZipFile


def read_kmx_file(kml_or_kmz_file):
    gpd.io.file.fiona.drvsupport.supported_drivers['KML'] = 'rw'

    if kml_or_kmz_file.endswith(".kmz"):
        kmz = ZipFile(kml_or_kmz_file, 'r')
        kml = kmz.open('doc.kml', 'r')
        df = gpd.read_file(kml, driver='KML')
    else:
        df = gpd.read_file(kml_or_kmz_file, driver='KML')
    df.geometry = df.geometry.map(
        lambda polygon: shapely.ops.transform(lambda x, y, z: (x, y), polygon))
    return df
