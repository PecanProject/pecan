#!/usr/bin/env python3
# -*- coding: utf-8 -*-

"""
remote_process controls the individual functions to create an automatic workflow for downloading and performing computation on remote sensing data.

Requires Python3

Author: Ayush Prasad
"""

from gee2pecan_bands import gee2pecan_bands
from bands2ndvi import bands2ndvi
from bands2lai_snap import bands2lai_snap
from satellitetools import gee
import geopandas as gpd


def remote_process(
    geofile,
    outdir,
    start,
    end,
    qi_threshold,
    source="gee",
    collection="COPERNICUS/S2_SR",
    input_type="bands",
    output=["lai", "ndvi"],
):
    """
    Parameters
    ----------
    geofile (str) -- path to the file containing the name and coordinates of ROI, currently tested with geojson. 
    
    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.
  
    start (str) -- starting date of the data request in the form YYYY-MM-DD
    
    end (str) -- ending date areaof the data request in the form YYYY-MM-DD
    
    qi_threshold (float) -- Threshold value to filter images based on used qi filter. qi filter holds labels of classes whose percentages within the AOI is summed. If the sum is larger then the qi_threshold, data will not be retrieved for that date/image. The default is 1, meaning all data is retrieved
        
    source (str) -- source from where data is to be downloaded

    collection (str) -- dataset ID

    input_type (str) -- type of raw intermediate data

    output (list of str) -- type of output data requested 

    Returns
    -------
    Nothing:
            output netCDF is saved in the specified directory.
            
    Python dependencies required: earthengine-api, geopandas, pandas, netCDF4, xarray

    To test this function run: python3 remote_process.py     
    """

    # this part will be removed in the next version, after deciding whether to pass the file or the extracted data to initial functions
    df = gpd.read_file(geofile)
    area = gee.AOI(df[df.columns[0]].iloc[0], df[df.columns[1]].iloc[0])

    # selecting the initial data download function by concatenating source and input_type
    initial_step = "".join([source, "2pecan", input_type])
    if initial_step == "gee2pecanbands":
        if collection == "COPERNICUS/S2_SR":
            gee2pecan_bands(geofile, outdir, start, end, qi_threshold)
        else:
            print("other gee download options go here, currently WIP")
            # This should be a function being called from an another file
            """
            data = ee.ImageCollection(collection)
            filtered_data = (data.filterDate(start, end).select(bands).filterBounds(ee.Geometry(pathtofile))
            filtered_data = filtered_data.getInfo()
            ...
            """

    else:
        print("other sources like AppEEARS  go here")
        return

    # if raw data is the requested output, process is completed  
    if input_type == output:
        print("process is complete")

    else:
        # locate the raw data file formed in initial step
        input_file = "".join([outdir, area.name, "_", str(input_type), ".nc"])

        # store all the requested conversions in a list 
        conversions = []
        for conv_type in output:
            conversions.append("".join([input_type, "2", conv_type]))

        # perform the available conversions
        if "bands2lai" in conversions:
            print("using SNAP to calculate LAI")
            bands2lai_snap(input_file, outdir)

        if "bands2ndvi" in conversions:
            print("using GEE to calculate NDVI")
            bands2ndvi(input_file, outdir)


if __name__ == "__main__":
    remote_process(
        "./satellitetools/test.geojson",
        "./out/",
        "2019-01-01",
        "2019-12-31",
        1,
        "gee",
        "COPERNICUS/S2_SR",
        "bands",
        ["lai", "ndvi"],
    )
