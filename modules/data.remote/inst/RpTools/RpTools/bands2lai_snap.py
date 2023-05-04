#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Calculates LAI using SNAP.

Author: Ayush Prasad
"""

import RpTools.gee2pecan_s2 as gee
from RpTools.gee2pecan_s2 import xr_dataset_to_timeseries
import RpTools.biophys_xarray as bio
from collections import OrderedDict
import geopandas as gpd
import xarray as xr
import numpy as np
import os
import time


def bands2lai_snap(inputfile, outdir, lat, lon, filename):
    """
    Calculates LAI for the input netCDF file and saves it in a new netCDF file.

    Parameters
    ----------
    input (str) -- path to the input netCDF file containing bands.
    
    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.

    lat  (float) -- latitude of the site

    lon (float) -- longitude of the site
    
    filename (str) -- filename of the output file

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

    # creating a timerseries
    timeseries[area.name] = xr_dataset_to_timeseries(area, timeseries_variable)

    area = area.rename_vars({"lai": "LAI_UNCOLLAPSED"})

    lat = np.array([lat])
    lon = np.array([lon])

    od = OrderedDict()
    od["x"] = "x"
    od["y"] = "y"

    latlon = OrderedDict()
    latlon["lat"] = lat
    latlon["lon"] = lon

    # aggregate the values
    LAI = area.LAI_UNCOLLAPSED.mean(dim=od)
    LAI = LAI.expand_dims(dim=latlon)
    LAI.attrs = {"units": "m2 m-2"}

    LAI_STD = area.LAI_UNCOLLAPSED.std(dim=od)
    LAI_STD = LAI_STD.expand_dims(dim=latlon)

    area = area.assign({"LAI": LAI, "LAI_STD": LAI_STD})

    save_path = os.path.join(outdir, filename + "_" + timestamp + ".nc")
    area.to_netcdf(save_path)

    return os.path.abspath(save_path)
