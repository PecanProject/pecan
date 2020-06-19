#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Calculates LAI using SNAP.

Author: Ayush Prasad
"""

from satellitetools import gee
import satellitetools.biophys_xarray as bio
import geopandas as gpd
import xarray as xr
import os


def bands2lai_snap(inputfile, outdir):
    """
    Calculates LAI for the input netCDF file and saves it in a new netCDF file.

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
    # calculate LAI using SNAP
    area = bio.run_snap_biophys(ds_disk, "LAI")

    timeseries = {}
    timeseries_variable = ["lai"]

    # if specified output directory does not exist, create it.    
    if not os.path.exists(outdir):
        os.makedirs(outdir, exist_ok=True)

    # creating a timerseries and saving the netCDF file
    area.to_netcdf(os.path.join(outdir, area.name + "_lai.nc"))
    timeseries[area.name] = gee.xr_dataset_to_timeseries(area, timeseries_variable)