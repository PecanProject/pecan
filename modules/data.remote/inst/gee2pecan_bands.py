#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
Downloads ESA Sentinel 2, Level-2A Bottom of Atmosphere data and saves it in a netCDF file.
Bands retrieved: B3, B4, B5, B6, B7, B8A, B11 and B12
More information about the bands and the process followed to get the data can be found out at /satellitetools/geeapi.py

Warning: Point coordinates as input has currently not been implemented.

Requires Python3

Uses satellitetools created by Olli Nevalainen.

Author: Ayush Prasad
"""

from satellitetools import gee
import geopandas as gpd
import os


def gee2pecan_bands(geofile, outdir, start, end, qi_threshold):
    """ 
    Downloads Sentinel 2 data from gee and saves it in a netCDF file at the specified location.
    
    Parameters
    ----------
    geofile (str) -- path to the file containing the name and coordinates of ROI, currently tested with geojson. 
    
    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.
  
    start (str) -- starting date of the data request in the form YYYY-MM-DD
    
    end (str) -- ending date of the data request in the form YYYY-MM-DD
    
    qi_threshold (float) -- From satellitetools: Threshold value to filter images based on used qi filter. qi filter holds labels of classes whose percentages within the AOI is summed. If the sum is larger then the qi_threshold, data will not be retrieved for that date/image. The default is 1, meaning all data is retrieved
        
    Returns
    -------
    Nothing:
            output netCDF is saved in the specified directory.
            
    Python dependencies required: earthengine-api, geopandas, pandas, netCDF4, xarray
    To test this function please run the following code inside a python shell after importing this module, testfile is included.

    gee2pecan_bands(geofile="./satellitetools/test.geojson", outdir="./out/", start="2019-01-01", end="2019-12-31", qi_threshold=1)       
    """

    # read in the input file containing coordinates
    df = gpd.read_file(geofile)

    request = gee.S2RequestParams(start, end)

    # filter area of interest from the coordinates in the input file
    area = gee.AOI(df[df.columns[0]].iloc[0], df[df.columns[1]].iloc[0])

    # calcuate qi attribute for the AOI
    gee.ee_get_s2_quality_info(area, request)

    # get the final data
    gee.ee_get_s2_data(area, request, qi_threshold=qi_threshold)

    # convert dataframe to an xarray dataset, used later for converting to netCDF
    gee.s2_data_to_xarray(area, request)

    # if specified output directory does not exist, create it
    if not os.path.exists(outdir):
        os.makedirs(outdir, exist_ok=True)

    # create a timerseries and save the netCDF file
    area.data.to_netcdf(os.path.join(outdir, area.name + "_bands.nc"))
