#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
get_remote_data controls GEE and AppEEARS functions to download data.

Requires Python3

Author(s): Ayush Prasad, Istem Fer
"""
from RpTools.merge_files import nc_merge, csv_merge
from importlib import import_module
from . appeears2pecan import appeears2pecan
import os
import os.path


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
    raw_merge=None,
    existing_raw_file_path=None,
    raw_file_name=None
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

    qc (float) -- quality control parameter, None by default
    
    credfile (str) -- path to credentials file only requried for AppEEARS, None by default

    raw_merge (str) -- if the existing raw file has to be merged, None by default
    
    existing_raw_file_path (str) -- path to exisiting raw file if raw_merge is TRUE., None by default
  
    raw_file_name (str) -- filename of the output file
  
    Returns
    -------
    Absolute path to the created file.
    output netCDF is saved in the specified directory.
    """


    if source == "gee":
        # construct the function name
        func_name = "".join([source, "2pecan", "_", collection])
        # import the module
        module_from_pack = "RpTools" + "." + func_name
        module = import_module(module_from_pack)
        # import the function from the module
        func = getattr(module, func_name)
        # if a qc parameter is specified pass these arguments to the function
        if qc:
            get_datareturn_path = func(geofile=geofile, outdir=outdir, start=start, end=end, scale=scale, qc=qc, filename=raw_file_name)
        # this part takes care of functions which do not perform any quality checks, e.g. SMAP
        else:
            get_datareturn_path = func(geofile=geofile, outdir=outdir, start=start, end=end, filename=raw_file_name)

    if source == "appeears":
        get_datareturn_path = appeears2pecan(geofile=geofile, outdir=outdir, out_filename=raw_file_name, start=start, end=end, product=collection, projection=projection, credfile=credfile)

    if raw_merge == True and raw_merge != "replace":
        # if output file is of csv type use csv_merge, example AppEEARS point AOI type 
        if os.path.splitext(existing_raw_file_path)[1][1:] == "csv":
            get_datareturn_path = csv_merge(existing_raw_file_path, get_datareturn_path, outdir)
        # else it must be of netCDF type, use nc_merge
        else:
            get_datareturn_path = nc_merge(existing_raw_file_path, get_datareturn_path, outdir)

    return get_datareturn_path

