"""
Downloads SMAP Global Soil Moisture Data from Google Earth Engine and saves it in a netCDF file.

Requires Python3

Author: Ayush Prasad
"""

import ee
import pandas as pd
import geopandas as gpd
import os
import xarray as xr
import datetime

ee.Initialize()


def gee2pecan_smap(geofile, outdir, start, end, var):
    """
    Downloads and saves SMAP data from GEE

    Parameters
    ----------
    geofile (str) -- path to the geosjon file containing the name and coordinates of ROI
    
    outdir (str) -- path to the directory where the output file is stored. If specified directory does not exists, it is created.

    start (str) -- starting date of the data request in the form YYYY-MM-dd

    end (str) -- ending date areaof the data request in the form YYYY-MM-dd

    var (str) -- one of ssm, susm, smp, ssma, susma

    Returns
    -------
    Nothing:
            output netCDF is saved in the specified directory
    """

    # read in the geojson file
    df = gpd.read_file(geofile)

    if (df.geometry.type == "Point").bool():
        # extract coordinates
        lon = float(df.geometry.x)
        lat = float(df.geometry.y)
        # create geometry
        geo = ee.Geometry.Point(lon, lat)

    elif (df.geometry.type == "Polygon").bool():
        # extract coordinates
        area = [
            list(df.geometry.exterior[row_id].coords) for row_id in range(df.shape[0])
        ]
        # create geometry
        geo = ee.Geometry.Polygon(area)

    else:
        # if the input geometry type is not
        raise ValueError("geometry type not supported")

    def smap_ts(geo, start, end, var):
        # extract a feature from the geometry
        features = [ee.Feature(geo)]
        # create a feature collection from the features
        featureCollection = ee.FeatureCollection(features)

        def smap_ts_feature(feature):
            area = feature.geometry()
            # create the image collection
            collection = (
                ee.ImageCollection("NASA_USDA/HSL/SMAP_soil_moisture")
                .filterBounds(area)
                .filterDate(start, end)
                .select([var])
            )

            def smap_ts_image(img):
                # scale (int) Default: 30
                scale = 30
                # extract date from the image
                dateinfo = ee.Date(img.get("system:time_start")).format("YYYY-MM-dd")
                # reduce the region to a list, can be configured as per requirements
                img = img.reduceRegion(
                    reducer=ee.Reducer.toList(),
                    geometry=area,
                    maxPixels=1e8,
                    scale=scale,
                )
                # store data in an ee.Array
                smapdata = ee.Array(img.get(var))
                tmpfeature = (
                    ee.Feature(ee.Geometry.Point([0, 0]))
                    .set("smapdata", smapdata)
                    .set("dateinfo", dateinfo)
                )
                return tmpfeature

            # map tmpfeature over the image collection
            smap_timeseries = collection.map(smap_ts_image)
            return feature.set(
                "smapdata", smap_timeseries.aggregate_array("smapdata")
            ).set("dateinfo", smap_timeseries.aggregate_array("dateinfo"))

        # map feature over featurecollection
        featureCollection = featureCollection.map(smap_ts_feature).getInfo()
        return featureCollection

    fc = smap_ts(geo=geo, start=start, end=end, var=var)

    def fc2dataframe(fc):
        smapdatalist = []
        datelist = []
        # extract var and date data from fc dictionary and store in it in smapdatalist and datelist
        for i in range(len(fc["features"][0]["properties"]["smapdata"])):
            smapdatalist.append(fc["features"][0]["properties"]["smapdata"][i][0])
            datelist.append(
                datetime.datetime.strptime(
                    (fc["features"][0]["properties"]["dateinfo"][i]).split(".")[0],
                    "%Y-%m-%d",
                )
            )
        fc_dict = {"date": datelist, var: smapdatalist}
        # create a pandas dataframe and store the data
        fcdf = pd.DataFrame(fc_dict, columns=["date", var])
        return fcdf

    datadf = fc2dataframe(fc)

    site_name = df[df.columns[0]].iloc[0]
    AOI = str(df[df.columns[1]].iloc[0])

    # convert the dataframe to an xarray dataset, used for converting it to a netCDF file
    tosave = xr.Dataset(
        datadf,
        attrs={
            "site_name": site_name,
            "start_date": start,
            "end_date": end,
            "AOI": AOI,
            "product": var,
        },
    )

    # # if specified output path does not exist create it
    if not os.path.exists(outdir):
        os.makedirs(outdir, exist_ok=True)

    file_name = "_" + var
    # convert to netCDF and save the file
    tosave.to_netcdf(os.path.join(outdir, site_name + file_name + ".nc"))
