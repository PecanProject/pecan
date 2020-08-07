#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Calculates LAI using SNAP.

Author: Ayush Prasad
"""

import RpTools.gee2pecan_s2 as gee
from RpTools.gee2pecan_s2 import xr_dataset_to_timeseries
import RpTools.biophys_xarray as bio
import geopandas as gpd
import xarray as xr
import os
import time

def bands2lai_snap(inputfile, outdir, siteid):
    """
    Calculates LAI for the input netCDF file and saves it in a new netCDF file.

    Parameters
    ----------
    input (str) -- path to the input netCDF file containing bands.
    
    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.
    
    siteid (str) -- shortform of the siteid
         
    Returns
    -------
    Absolute path to the output file
    output netCDF is saved in the specified directory.

    """
    # load the input file
    ds_disk = xr.open_dataset(inputfile)
    # select the required bands
    ds_disk = ds_disk.sel(band=["B3", "B4", "B5", "B6", "B7", "B8A", "B11", "B12"])
    # calculate LAI using SNAP
    area = bio.run_snap_biophys(ds_disk, "LAI")
    area = area[["lai"]]

    timeseries = {}
    timeseries_variable = ["lai"]

    # if specified output directory does not exist, create it.    
    if not os.path.exists(outdir):
        os.makedirs(outdir, exist_ok=True)

    timestamp = time.strftime("%y%m%d%H%M%S")
    
    if siteid is None:
        siteid = area.name

    save_path = os.path.join(outdir, "snap_lai_site_" + siteid + "_" + timestamp + ".nc")
    # creating a timerseries and saving the netCDF file
    area.to_netcdf(save_path)
    timeseries[area.name] = xr_dataset_to_timeseries(area, timeseries_variable)
    
    return os.path.abspath(save_path)
