#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Calculates NDVI using gee.

Author: Ayush Prasad
"""

import xarray as xr
from satellitetools import gee
import geopandas as gpd
import os


def bands2ndvi(inputfile, outdir):
    """
    Calculates NDVI for the input netCDF file and saves it in a new netCDF file.

    Parameters
    ----------
    input (str) -- path to the input netCDF file containing bands.
    
    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.
         
    Returns
    -------
    Nothing:
            output netCDF is saved in the specified directory.

    """
    # load the input file
    ds_disk = xr.open_dataset(inputfile)
    # calculate NDVI using gee
    area = gee.compute_ndvi(ds_disk)

    timeseries = {}
    timeseries_variable = ["ndvi"]

    # if specified output directory does not exist, create it.
    if not os.path.exists(outdir):
        os.makedirs(outdir, exist_ok=True)

    # creating a timerseries and saving the netCDF file
    area.to_netcdf(os.path.join(outdir, area.name + "_ndvi.nc"))
    timeseries[area.name] = gee.xr_dataset_to_timeseries(area, timeseries_variable)
