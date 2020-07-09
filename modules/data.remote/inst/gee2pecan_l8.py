"""
Extracts Landsat 8 surface reflactance band data from Google Earth Engine and saves it in a netCDF file

Requires Python3

Bands retrieved: B1, B2, B3, B4, B5, B6, B7, B10, B11 along with computed NDVI

If ROI is a Point, this function can be used for getting SR data from Landsat 7, 5 and 4 as well.

Author: Ayush Prasad
"""
from gee_utils import create_geo, get_sitecoord, get_sitename, calc_ndvi
import ee
import pandas as pd
import geopandas as gpd
import datetime
import os
import xarray as xr
import numpy as np
import re

ee.Initialize()


def gee2pecan_l8(geofile, outdir, start, end, scale, qc=1):
    """
    Extracts Landsat 8 SR band data from GEE

    Parameters
    ----------
    geofile (str) -- path to the file containing the name and coordinates of ROI, currently tested with geojson. 
    
    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.
  
    start (str) -- starting date of the data request in the form YYYY-MM-DD
    
    end (str) -- ending date areaof the data request in the form YYYY-MM-DD

    scale (int) -- pixel resolution

    qc (bool) -- uses the cloud masking function if set to True

    Returns
    -------
    Nothing:
            output netCDF is saved in the specified directory. 
 
    """

    def reduce_region(image):
        """
        Reduces the selected region
        currently set to mean, can be changed as per requirements.
        """
        stat_dict = image.reduceRegion(ee.Reducer.mean(), geo, scale)
        sensingtime = image.get("SENSING_TIME")
        return ee.Feature(None, stat_dict).set("sensing_time", sensingtime)

    def mask(image):
        """
        Masks clouds and cloud shadows using the pixel_qa band
        Can be configured as per requirements 
        """
        clear = image.select("pixel_qa")
        return image.updateMask(clear)

    # create image collection depending upon the qc choice
    if qc == True:
        landsat = (
            ee.ImageCollection("LANDSAT/LC08/C01/T1_SR")
            .filterDate(start, end)
            .map(mask)
            .sort("system:time_start", True)
        )

    else:
        landsat = (
            ee.ImageCollection("LANDSAT/LC08/C01/T1_SR")
            .filterDate(start, end)
            .sort("system:time_start", True)
        )

    # map NDVI to the image collection and select the bands
    landsat = landsat.map(calc_ndvi(nir="B5", red="B4")).select(
        ["B1", "B2", "B3", "B4", "B5", "B6", "B7", "B10", "B11", "NDVI"]
    )

    # if ROI is a point
    df = gpd.read_file(geofile)
    if (df.geometry.type == "Point").bool():

        geo = create_geo(geofile)

        # get the data
        l_data = landsat.filterBounds(geo).getRegion(geo, scale).getInfo()
        # put the data inside a list of dictionary
        l_data_dict = [dict(zip(l_data[0], values)) for values in l_data[1:]]

        def getdate(filename):
            """
            calculates Date from the landsat id
            """
            string = re.compile(
                r"(?P<version>LC08|LE07|LT05|LT04)_(?P<path>\d{6})_(?P<date>\d{8})"
            )
            x = string.search(filename)
            d = datetime.datetime.strptime(x.group("date"), "%Y%m%d").date()
            return d

        # pop out unnecessary keys and add date
        for d in l_data_dict:
            d.pop("longitude", None)
            d.pop("latitude", None)
            d.pop("time", None)
            d.update(time=getdate(d["id"]))

        # Put data in a dataframe
        datadf = pd.DataFrame(l_data_dict)
        # converting date to the numpy date format
        datadf["time"] = datadf["time"].apply(lambda x: np.datetime64(x))

    # if ROI is a polygon
    elif (df.geometry.type == "Polygon").bool():

        geo = create_geo(geofile)

        # get the data
        l_data = landsat.filterBounds(geo).map(reduce_region).getInfo()

        def l8_fc2dict(fc):
            """
            Converts feature collection to dictionary form.
            """

            def eachf2dict(f):
                id = f["id"]
                out = f["properties"]
                out.update(id=id)
                return out

            out = [eachf2dict(x) for x in fc["features"]]
            return out

        # convert to dictionary
        l_data_dict = l8_fc2dict(l_data)
        # create a dataframe from the dictionary
        datadf = pd.DataFrame(l_data_dict)
        # convert date to a more readable format
        datadf["sensing_time"] = datadf["sensing_time"].apply(
            lambda x: datetime.datetime.strptime(x.split(".")[0], "%Y-%m-%dT%H:%M:%S")
        )
    # if ROI type is not a Point ot Polygon
    else:
        raise ValueError("geometry choice not supported")

    site_name = get_sitename(geofile)
    AOI = get_sitecoord(geofile)
    # convert the dataframe to an xarray dataset
    tosave = xr.Dataset(
        datadf,
        attrs={
            "site_name": site_name,
            "start_date": start,
            "end_date": end,
            "AOI": AOI,
            "sensor": "LANDSAT/LC08/C01/T1_SR",
        },
    )

    # if specified path does not exist create it
    if not os.path.exists(outdir):
        os.makedirs(outdir, exist_ok=True)

    # convert the dataset to a netCDF file and save it
    tosave.to_netcdf(os.path.join(outdir, site_name + "_bands.nc"))
