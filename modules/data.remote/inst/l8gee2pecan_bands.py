"""
Extracts Landsat 8 surface reflactance band data from Google Earth Engine and saves it in a netCDF file

Requires Python3

If ROI is a Point, this function can be used for getting SR data from Landsat 7, 5 and 4 as well.

Author: Ayush Prasad
"""

import ee
import pandas as pd
import datetime
import geopandas as gpd
import os
import xarray as xr
import numpy as np
import re

ee.Initialize()


def l8gee2pecan_bands(geofile, outdir, start, end, ic, vi, qc, bands=["B5", "B4"]):
    """
    Extracts Landsat 8 SR band data from GEE

    Parameters
    ----------
    geofile (str) -- path to the file containing the name and coordinates of ROI, currently tested with geojson. 
    
    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.
  
    start (str) -- starting date of the data request in the form YYYY-MM-DD
    
    end (str) -- ending date areaof the data request in the form YYYY-MM-DD

    ic (str) -- image collection id of the Landsat SR product from Google Earth Engine

    vi (bool) -- set to True if NDVI needs to be calculated

    qc (bool) -- uses the cloud masking function if set to True

    bands (list of str) -- bands to be retrieved. Default: B5, B4

    Returns
    -------
    Nothing:
            output netCDF is saved in the specified directory. 
 
    """

    # read in the geojson file
    df = gpd.read_file(geofile)

    # scale (int) Default: 30
    scale = 30

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
            ee.ImageCollection(ic)
            .filterDate(start, end)
            .map(mask)
            .sort("system:time_start", True)
        )

    else:
        landsat = (
            ee.ImageCollection(ic)
            .filterDate(start, end)
            .sort("system:time_start", True)
        )

    # If NDVI is to be calculated select the appropriate bands and create the image collection
    if vi == True:

        def calcNDVI(image):
            """
            Calculates NDVI and adds the band to the image.
            """
            ndvi = image.normalizedDifference(["B5", "B4"]).rename("NDVI")
            return image.addBands(ndvi)

        # map NDVI to the image collection and select the NDVI band
        landsat = landsat.map(calcNDVI).select("NDVI")
        file_name = "_l8ndvi"

    # select the user specified bands if NDVI is not be calculated
    else:
        landsat = landsat.select(bands)
        file_name = "_l8bands"

    # if ROI is a point
    if (df.geometry.type == "Point").bool():
        # extract the coordinates
        lon = float(df.geometry.x)
        lat = float(df.geometry.y)
        # create geometry
        geo = ee.Geometry.Point(lon, lat)

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
        # extract coordinates
        area = [
            list(df.geometry.exterior[row_id].coords) for row_id in range(df.shape[0])
        ]
        geo = ee.Geometry.Polygon(area)

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

    site_name = df[df.columns[0]].iloc[0]
    AOI = str(df[df.columns[1]].iloc[0])
    # convert the dataframe to an xarray dataset
    tosave = xr.Dataset(
        datadf,
        attrs={
            "site_name": site_name,
            "start_date": start,
            "end_date": end,
            "AOI": AOI,
            "sensor": ic,
        },
    )

    # if specified path does not exist create it
    if not os.path.exists(outdir):
        os.makedirs(outdir, exist_ok=True)

    # convert the dataset to a netCDF file and save it
    tosave.to_netcdf(os.path.join(outdir, site_name + file_name + ".nc"))


if __name__ == "__main__":
    l8gee2pecan_bands(
        "./satellitetools/test.geojson",
        "./out/",
        "2018-01-01",
        "2018-12-31",
        "LANDSAT/LC08/C01/T1_SR",
        True,
        True,
    )
