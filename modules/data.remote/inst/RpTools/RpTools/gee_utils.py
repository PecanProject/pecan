#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
GEE utility functions

Requires Python3

Author: Ayush Prasad
"""

import ee
import geopandas as gpd


def create_geo(geofile):
    """
    creates a GEE geometry from the input file

    Parameters
    ----------
    geofile (str) -- path to the file containing the name and coordinates of ROI, currently tested with geojson. 

    Returns
    -------
    geo -- object of ee.Geometry type
    """
    df = gpd.read_file(geofile)
    if (df.geometry.type == "Point").bool():
        # extract coordinates
        lon = float(df.geometry.x)
        lat = float(df.geometry.y)
        # create geometry
        geo = ee.Geometry.Point(lon, lat)

    elif (df.geometry.type == "Polygon").bool():
        # extract coordinates
        area = [
            list(df.geometry.exterior[row_id].coords) for row_id in range(df.shape[0])
        ]
        # create geometry
        for i in range(len(area[0])):
             area[0][i] = area[0][i][0:2]
        geo = ee.Geometry.Polygon(area)
        
    else:
        # if the input geometry type is not
        raise ValueError("geometry type not supported")

    return geo


def get_sitename(geofile):
    """
    extracts AOI name from the input file

    Parameters
    ----------
    geofile (str) -- path to the file containing the name and coordinates of ROI, currently tested with geojson. 

    Returns
    -------
    site_name (str) -- name of the AOI
    """
    df = gpd.read_file(geofile)
    site_name = df[df.columns[0]].iloc[0]
    return site_name


def get_sitecoord(geofile):
    """
    extracts AOI coordinates from the input file

    Parameters
    ----------
    geofile (str) -- path to the file containing the name and coordinates of ROI, currently tested with geojson. 

    Returns
    -------
    site_aoi (str) -- coordinates of the AOI
    """
    df = gpd.read_file(geofile)
    site_aoi = str(df[df.columns[1]].iloc[0])
    return site_aoi

def calc_ndvi(nir, red):
    """
    calculates NDVI on GEE

    Parameters
    ----------
    nir (str) -- NIR band of the image collection

    red (str) -- RED band of the image collection

    Returns
    -------
    image -- with added NDVI band

    """
    def add_ndvi(image):
        ndvi = image.normalizedDifference([nir, red]).rename("NDVI")
        return image.addBands(ndvi)
    return add_ndvi
