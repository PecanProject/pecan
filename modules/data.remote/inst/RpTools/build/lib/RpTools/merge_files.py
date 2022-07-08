#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Used to merge netCDF and CSV files
Author: Ayush Prasad
"""

import xarray
import os
import time
import pandas as pd


def nc_merge(old, new, outdir):
    """
    Merge netCDF files. 
    Order in which the files are specified in the function does not matter.
    
    Parameters
    ----------
    old (str) -- path to the first netCDF file
    new (str) -- path to the second netCDF file
    outdir (str) -- path where the merged file has to be stored
    Returns
    -------
    Absolute path to the merged file
    output netCDF file is saved in the specified directory.
    """
    # extract the name from the new (second) netCDF file and attach timestamp to it, this will be the name of the output merged file
    head, tail = os.path.split(new)
    orig_nameof_newfile = new
    timestamp = time.strftime("%y%m%d%H%M%S")
    changed_new = os.path.join(outdir, tail + "temp" + timestamp + ".nc")
    # rename the new file to prevent it from being overwritten
    os.rename(orig_nameof_newfile, changed_new)
    try:
        ds = xarray.open_mfdataset([old, changed_new], combine="by_coords")
        ds.to_netcdf(os.path.join(outdir, tail))
    except:
        os.remove(old)
        return os.path.abspath(os.path.join(outdir, changed_new))
    # delete the old and temproary file
    os.remove(changed_new)
    os.remove(old)
    return os.path.abspath(os.path.join(outdir, tail))


def csv_merge(old, new, outdir):
    """
    Merge csv files. 
    Order in which the files are specified in the function does not matter.
    
    Parameters
    ----------
    old (str) -- path to the first csv file
    new (str) -- path to the second csv file
    outdir (str) -- path where the merged file has to be stored
    Returns
    -------
    Absolute path to the merged file
    output csv file is saved in the specified directory.
    """

    # extract the name from the new (second) csv file and attach timestamp to it, this will be the name of the output merged file
    head, tail = os.path.split(new)
    orig_nameof_newfile = new
    timestamp = time.strftime("%y%m%d%H%M%S")
    changed_new = os.path.join(outdir, tail + "temp" + timestamp + ".csv")
    # rename the new file to prevent it from being overwritten
    os.rename(orig_nameof_newfile, changed_new)
    df_old = pd.read_csv(old)
    df_changed_new = pd.read_csv(changed_new)
    merged_df = pd.concat([df_old, df_changed_new])
    merged_df = merged_df.sort_values(by="Date")
    merged_df.to_csv(os.path.join(outdir, tail), index=False)
    # delete the old and temproary file
    os.remove(changed_new)
    os.remove(old)
    return os.path.abspath(os.path.join(outdir, tail))
