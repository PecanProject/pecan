#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
get_remote_data controls GEE and AppEEARS functions to download data.

Requires Python3

Author(s): Ayush Prasad, Istem Fer
"""

from importlib import import_module
from appeears2pecan import appeears2pecan

# dictionary used to map the GEE image collection id to PEcAn specific function name
collection_dict = {
    "LANDSAT/LC08/C01/T1_SR": "l8",
    "COPERNICUS/S2_SR": "s2",
    "NASA_USDA/HSL/SMAP_soil_moisture": "smap",
    # "insert GEE collection id": "insert PEcAn specific name",
}


def get_remote_data(
    geofile,
    outdir,
    start,
    end,
    source,
    collection,
    scale=None,
    projection=None,
    qc=None,
    credfile=None,
):
    """
    uses GEE and AppEEARS functions to download data

    Parameters
    ----------
    geofile (str) -- path to the file containing the name and coordinates of ROI, currently tested with geojson. 
    
    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.
  
    start (str) -- starting date of the data request in the form YYYY-MM-DD
    
    end (str) -- ending date areaof the data request in the form YYYY-MM-DD

    source (str) -- source from where data is to be downloaded

    collection (str) -- dataset or product name as it is provided on the source, e.g. "COPERNICUS/S2_SR" for gee or "SPL3SMP_E.003" for appeears

    scale (int) -- pixel resolution, None by default

    projection (str) -- type of projection. Only required for appeears polygon AOI type. None by default. 

    qc (float) -- quality control parameter

    Returns
    -------
    Nothing:
            output netCDF is saved in the specified directory.
    """
    if source == "gee":
        try:
            # get collection id from the dictionary
            collection = collection_dict[collection]
        except KeyError:
            print(
                "Please check if the collection name you requested is one of these and spelled correctly. If not, you need to implement a corresponding gee2pecan_{} function and add it to the collection dictionary.".format(
                    collection
                )
            )
            print(collection_dict.keys())
        # construct the function name
        func_name = "".join([source, "2pecan", "_", collection])
        # import the module
        module = import_module(func_name)
        # import the function from the module
        func = getattr(module, func_name)
        # if a qc parameter is specified pass these arguments to the function
        if qc:
            func(geofile, outdir, start, end, scale, qc)
        # this part takes care of functions which do not perform any quality checks, e.g. SMAP
        else:
            func(geofile, outdir, start, end)

    if source == "appeears":
        appeears2pecan(geofile, outdir, start, end, collection, projection, credfile)
